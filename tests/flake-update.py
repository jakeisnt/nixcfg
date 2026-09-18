import os, pathlib, subprocess, tempfile
script=str(pathlib.Path(__file__).resolve().parents[1] / 'bin/flake-update')
def run(*args, **kwargs):
    return subprocess.run(args, check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, **kwargs)
for mode in ['success','unchanged','failure','unexpected']:
    with tempfile.TemporaryDirectory() as tmp:
        root=pathlib.Path(tmp); repo=root/'repo'; remote=root/'remote'; tools=root/'bin'; tools.mkdir()
        run('git','init','--bare',str(remote)); run('git','init','-b','main',str(repo))
        run('git','-C',str(repo),'config','user.name','Test'); run('git','-C',str(repo),'config','user.email','test@example.com')
        (repo/'flake.lock').write_text('old\n'); run('git','-C',str(repo),'add','.'); run('git','-C',str(repo),'commit','-m','initial')
        run('git','-C',str(repo),'remote','add','origin',str(remote)); run('git','-C',str(repo),'push','origin','main')
        (repo/'local-edit').write_text('preserve me')
        (tools/'nix').write_text('''#!/usr/bin/env bash
set -eu
case "$1 $2" in
  'flake update') if [[ $MODE != unchanged ]]; then echo new > flake.lock; fi ;;
  'flake check') if [[ $MODE == failure ]]; then exit 42; fi ;;
  'eval --json') echo '["work","xps"]' ;;
  build*) echo "$*" >> "$BUILD_LOG"; if [[ $MODE == unexpected ]]; then touch surprise; fi ;;
  *) exit 99 ;;
esac
''')
        (tools/'notify-send').write_text('#!/usr/bin/env bash\nexit 0\n')
        for p in tools.iterdir(): p.chmod(0o755)
        env=dict(os.environ, PATH=str(tools)+':'+os.environ['PATH'], STATE_DIRECTORY=str(root/'state'), MODE=mode, BUILD_LOG=str(root/'builds'))
        result=subprocess.run(['bash',script,str(repo),'main'],env=env,capture_output=True,text=True)
        assert (result.returncode==0)==(mode in ['success','unchanged']),result.stderr
        assert (repo/'flake.lock').read_text()=='old\n'
        assert (repo/'local-edit').read_text()=='preserve me'
        refs=run('git','--git-dir',str(remote),'for-each-ref','--format=%(refname)').stdout
        assert ('refs/heads/updates/' in refs)==(mode=='success'),refs
        assert (root/'state/worktree').exists()==(mode in ['failure','unexpected'])
        if mode=='success': assert len((root/'builds').read_text().splitlines())==2
        if mode=='failure':
            again=subprocess.run(['bash',script,str(repo),'main'],env=env,capture_output=True,text=True)
            assert again.returncode!=0 and 'Previous worktree exists' in again.stdout
        print(mode+': PASS')
