env | each { |it| echo $"let-env ($it.name) = '($it.raw)'" } | str join (char nl)

exec sway
