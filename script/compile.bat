@echo off

if [%GRAALVM_HOME%] == [] (
  echo Please set GRAALVM_HOME
  exit /b
)

if not exist "%GRAALVM_HOME%\bin\native-image.cmd" (
  call "%GRAALVM_HOME%\bin\gu.cmd" install native-image
  if %errorlevel% neq 0 exit /b %errorlevel%
)

for /f "delims=" %%x in (resources/SCI_VERSION) do set SCI_VERSION=%%x

set JAVA_HOME=%GRAALVM_HOME%
set PATH=%GRAALVM_HOME%/bin;%PATH%

call lein with-profiles +native-image do clean, uberjar
if %errorlevel% neq 0 exit /b %errorlevel%

set SCI_JAR=target\sci-%SCI_VERSION%-standalone.jar

call "%GRAALVM_HOME%\bin\native-image.cmd"% ^
  -jar %SCI_JAR% ^
  -cp src-java-graalvm ^
  -H:Name=sci ^
  -H:+ReportExceptionStackTraces ^
  -J-Dclojure.spec.skip-macros=true ^
  -J-Dclojure.compiler.direct-linking=true ^
  "-H:IncludeResources=SCI_VERSION" ^
  -H:ReflectionConfigurationFiles=reflection.json ^
  --initialize-at-run-time=java.lang.Math\$RandomNumberGeneratorHolder ^
  --initialize-at-build-time  ^
  -H:Log=registerResource: ^
  --verbose ^
  --no-fallback ^
  --no-server ^
  "-J-Xmx3g"
if %errorlevel% neq 0 exit /b %errorlevel%

call lein clean
if %errorlevel% neq 0 exit /b %errorlevel%
