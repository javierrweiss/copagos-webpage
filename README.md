# Página web para carga dinámica de copagos (Doc24)

## Motivación

Carga y actualización regular de los montos de copago para teleconsultas por obra, plan y especialidad.

Esta aplicación guarda en las tablas scweb.tbl_planes_obras_sociales y scweb.tbl_copago_historico, la primera de cuales es leída por el cliente de Doc24
para levantar turnos de teleconsulta a través de la plataforma en cuestión.

## Uso

Se trata de un formulario simple donde el usuario introduce un código de obra social, uno o más planes separados por comas, selecciona una o más especialidades y, por último, el copago correspondiente.


## Desarrollo

```bash
bb dev
```

Luego en el IDE conectar un REPL nbb corriendo en tu proyecto en el puerto 1339

## Producción

Es necesario tener instalado Babashka https://babashka.org/

```bash
bb prod
```

## TODOS

- Implementar guardado en ambas tablas, de tal modo que:
    * Si la fecha de vigencia es igual o menor a la actual se guarden los registros correspondientes en ambas tablas.
    * Si la fecha de vigencia es superior a la actual, sólo se guarde en scweb.tbl_copago_historico.
- Implementar alguna clase de scheduler que todos los días a cierta hora busque si existe un registro en scweb.tbl_copago_historico cuya fecha `vigente_desde` sea igual a la fecha actual y lo inserte en scweb.tbl_planes_obras_sociales.
- Crear UI para visualización de histórico
- Crear UI para visualización del estado actual de la tabla (se me ocurre que cada uno de estos elementos los dejemos invisibles en el DOM y luego que un botón en la barra superior los haga visibles)
- Agregar fecha de vigencia en el formulario ✅
- Procesar planes antes de enviar al backend ✅
- Arreglar ingreso de datos a tablas según nuevo input 
- Agregar validaciones de datos y campos obligatorios ✅   