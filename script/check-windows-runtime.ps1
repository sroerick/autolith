# Exercise the staged runtime, long-path stat, and core save/reload on Unicode paths.
param([Parameter(Mandatory = $true)][string]$RuntimeDirectory)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$RuntimeDirectory = (Resolve-Path -LiteralPath $RuntimeDirectory).ProviderPath
$testId = [Guid]::NewGuid().ToString('N')
$runtime = Join-Path $RuntimeDirectory 'sbcl.exe'
$core = Join-Path $RuntimeDirectory "unicode-test-$testId.core"
$shim = Join-Path $PSScriptRoot 'windows-stat.lisp'
$fixture = Join-Path $RuntimeDirectory "stat-test-$testId"
$names = @('SBCL_HOME', 'AUTOLITH_RUNTIME_TEST_CORE', 'AUTOLITH_STAT_TEST_BASE',
           'AUTOLITH_STAT_TEST_ASCII', 'AUTOLITH_STAT_TEST_UNICODE')
$previous = @{}
foreach ($name in $names) { $previous[$name] = [Environment]::GetEnvironmentVariable($name) }
try {
  $env:SBCL_HOME = $RuntimeDirectory
  $env:AUTOLITH_RUNTIME_TEST_CORE = $core
  $env:AUTOLITH_STAT_TEST_BASE = $RuntimeDirectory
  foreach ($kind in @('ASCII', 'UNICODE')) {
    $directory = $fixture
    $segment = if ($kind -eq 'ASCII') { 'a' * 65 } else { 'Lukáš-žluťoučký-😀' * 4 }
    foreach ($index in 1..8) { $directory = Join-Path $directory "$segment-$index" }
    # Extended native names also let older PowerShell hosts create the fixture.
    [IO.Directory]::CreateDirectory(('\\?\' + $directory)) | Out-Null
    $file = Join-Path $directory 'data.bin'
    [IO.File]::WriteAllBytes(('\\?\' + $file), [byte[]](1, 2, 3, 4, 5))
    [Environment]::SetEnvironmentVariable("AUTOLITH_STAT_TEST_$kind", $file)
  }
  $save = @'
(progn
  (defun autolith-runtime--test-stat ()
    (dolist (variable '("AUTOLITH_STAT_TEST_ASCII" "AUTOLITH_STAT_TEST_UNICODE"))
      (let* ((file (uiop:getenv variable))
             (directory (subseq file 0 (- (length file) (length "data.bin"))))
             (base (uiop:getenv "AUTOLITH_STAT_TEST_BASE"))
             (relative (subseq file (1+ (length base))))
             (root (subseq file 0 3)))
        (assert (> (length (sb-ext:string-to-octets file :external-format :utf-8)) 260))
        (dolist (stat '(sb-unix:unix-stat sb-unix:unix-lstat))
          (dolist (name (list file relative
                             (concatenate 'string (subseq file 0 2) relative)
                             (subseq file 2)
                             (concatenate 'string "\\\\?\\" file)
                             (substitute #\/ #\\ file)))
            (let ((result (multiple-value-list (funcall stat name))))
              (assert (= 14 (length result)))
              (assert (first result))
              (assert (= #o100000 (logand #o170000 (fourth result))))
              (assert (= 5 (nth 8 result)))
              (dolist (index '(2 5 6 12 13)) (assert (zerop (nth index result))))
              (dolist (index '(9 10 11)) (assert (integerp (nth index result))))))
          (dolist (name (list directory (string-right-trim "\\" directory) root))
            (let ((result (multiple-value-list (funcall stat name))))
              (assert (= 14 (length result)))
              (assert (first result))
              (assert (= #o40000 (logand #o170000 (fourth result))))))
          (dolist (name (list (concatenate 'string directory "missing") "" "C:"))
            (let ((result (multiple-value-list (funcall stat name))))
              (assert (= 2 (length result)))
              (assert (null (first result)))
              (assert (= 2 (second result))))))))
    (dolist (name '("C:\\" "\\\\?\\C:\\"))
      (let ((result (multiple-value-list (sb-unix:unix-stat name))))
        (assert (first result))
        (assert (= 2 (second result)))
        (assert (= 2 (nth 7 result))))))
  (assert (= 65001 (sb-alien:alien-funcall
                   (sb-alien:extern-alien "GetACP"
                                          (function sb-alien:unsigned-int)))))
  (autolith-runtime--test-stat)
  (assert (probe-file sb-ext:*runtime-pathname*))
  (assert (probe-file (sb-int:sbcl-homedir-pathname)))
  (sb-ext:save-lisp-and-die
   (uiop:getenv "AUTOLITH_RUNTIME_TEST_CORE")
   :executable nil
   :toplevel (lambda ()
               (autolith-runtime--test-stat)
               (assert (probe-file sb-ext:*core-pathname*))
               (assert (equal (uiop:parse-native-namestring
                               (uiop:getenv "AUTOLITH_RUNTIME_TEST_CORE"))
                              sb-ext:*core-pathname*))
               (format t "Unicode runtime, long-path stat and core round-trip passed.~%")
               (sb-ext:exit :code 0))))
'@
  Push-Location $RuntimeDirectory
  try {
    & $runtime --noinform --non-interactive --load $shim --eval '(require :asdf)' --eval $save
    if ($LASTEXITCODE -ne 0) { throw 'Unicode runtime initialization or core save failed.' }
    & $runtime --noinform --core $core --end-runtime-options
    if ($LASTEXITCODE -ne 0) { throw 'Unicode core reload failed.' }
  } finally { Pop-Location }
} finally {
  foreach ($name in $names) { [Environment]::SetEnvironmentVariable($name, $previous[$name]) }
  if (Test-Path -LiteralPath $core) { Remove-Item -Force -LiteralPath $core }
  if ([IO.Directory]::Exists(('\\?\' + $fixture))) {
    [IO.Directory]::Delete(('\\?\' + $fixture), $true)
  }
}
