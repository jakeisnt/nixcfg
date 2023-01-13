env | each { |it| echo $"let-env ($it.name) = '($it.raw)'" } | str join (char nl)

if (echo (tty)) == /dev/tty1 {
  exec sway
}
