name := "Bot-tender"

version := "0.1"

scalaVersion := "2.13.5"

javaOptions in run := List(
// when starting wait for debugger to attach
"-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
)
fork in run := true
outputStrategy := Some(StdoutOutput) // directly write output in console
connectInput in run := true