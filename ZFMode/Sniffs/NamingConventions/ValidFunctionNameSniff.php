<?php
class ZFMode_Sniffs_NamingConventions_ValidFunctionNameSniff
    extends PEAR_Sniffs_NamingConventions_ValidFunctionNameSniff
{
    /**
     * Processes the tokens within the scope.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being processed.
     * @param int                  $stackPtr  The position where this token was
     *                                        found.
     * @param int                  $currScope The position of the current scope.
     *
     * @return void
     */
    protected function processTokenWithinScope(PHP_CodeSniffer_File $phpcsFile,
        $stackPtr, $currScope
    ) {
        $methodName = $phpcsFile->getDeclarationName($stackPtr);
        if ($methodName === null) {
            // Ignore closures.
            return;
        }

        $className = $phpcsFile->getDeclarationName($currScope);
        $errorData = array($className.'::'.$methodName);

        // Is this a magic method. IE. is prefixed with "__".
        if (preg_match('|^__|', $methodName) !== 0) {
            $magicPart = substr($methodName, 2);
            if (in_array($magicPart, $this->magicMethods) === false) {
                 $error = 'Method name "%s" is invalid; only PHP magic ' .
                     'methods should be prefixed with a double underscore';
                 
                 $phpcsFile->addError(
                     $error,
                     $stackPtr,
                     'MethodDoubleUnderscore',
                     $errorData
                 );
            }

            return;
        }

        // PHP4 constructors are allowed to break our rules.
        if ($methodName === $className) {
            return;
        }

        // PHP4 destructors are allowed to break our rules.
        if ($methodName === '_'.$className) {
            return;
        }

        $methodProps    = $phpcsFile->getMethodProperties($stackPtr);
        $isPublic       = ($methodProps['scope'] === 'public');
        $isPrivate      = ($methodProps['scope'] === 'private');
        $scope          = $methodProps['scope'];
        $scopeSpecified = $methodProps['scope_specified'];

        // If it's a private method, it must have an underscore on the front.
        if ($isPrivate === true && $methodName[0] !== '_') {
            $error = 'Private method name "%s" must be prefixed with an '
                . 'underscore';
            $phpcsFile->addError(
                $error,
                $stackPtr,
                'PrivateNoUnderscore',
                $errorData
            );
            return;
        }

        // If it's not a private method, it must not have an
        // underscore on the front.
        if ($isPublic === true && $scopeSpecified === true
            && $methodName[0] === '_'
        ) {
            $error = 'Public method name "%s" must not be prefixed with an '
                . 'underscore';
            $data  = array($errorData[0]);
            $phpcsFile->addError($error, $stackPtr, 'PublicUnderscore', $data);
            return;
        }

        // If the scope was specified on the method, then the method must be
        // camel caps and an underscore should be checked for. If it wasn't
        // specified, treat it like a public method and remove the underscore
        // prefix if there is one because we cant determine if it is private or
        // public.
        $testMethodName = $methodName;
        if ((!($isPublic || $isPrivate) || $scopeSpecified === false)
            && $methodName[0] === '_'
        ) {
            $testMethodName = substr($methodName, 1);
        }

        $isCamelCaps = PHP_CodeSniffer::isCamelCaps(
            $testMethodName,
            false,
            !$isPrivate,
            false
        );
        
        if ($isCamelCaps === false) {
            if ($scopeSpecified === true) {
                $error = '%s method name "%s" is not in camel caps format';
                $data  = array(
                          ucfirst($scope),
                          $errorData[0],
                         );
                $phpcsFile->addError(
                    $error,
                    $stackPtr,
                    'ScopeNotCamelCaps',
                    $data
                );
            } else {
                $error = 'Method name "%s" is not in camel caps format';
                $phpcsFile->addError(
                    $error,
                    $stackPtr,
                    'NotCamelCaps',
                    $errorData
                );
            }

            return;
        }

    }//end processTokenWithinScope()


    /**
     * Processes the tokens outside the scope.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being processed.
     * @param int                  $stackPtr  The position where this token was
     *                                        found.
     *
     * @return void
     */
    protected function processTokenOutsideScope(PHP_CodeSniffer_File $phpcsFile,
        $stackPtr
    ) {
        $functionName = $phpcsFile->getDeclarationName($stackPtr);
        if ($functionName === null) {
            // Ignore closures.
            return;
        }

        $errorData = array($functionName);

        // Is this a magic function. IE. is prefixed with "__".
        if (preg_match('|^__|', $functionName) !== 0) {
            $magicPart = substr($functionName, 2);
            if (in_array($magicPart, $this->magicFunctions) === false) {
                 $error = 'Function name "%s" is invalid; only PHP magic '
                     . 'methods should be prefixed with a double underscore';
                 $phpcsFile->addError(
                     $error,
                     $stackPtr,
                     'FunctionDoubleUnderscore',
                     $errorData
                 );
            }

            return;
        }

        // Function names can be in two parts; the package name and
        // the function name.
        $packagePart   = '';
        $camelCapsPart = '';
        $underscorePos = strrpos($functionName, '_');
        if ($underscorePos === false) {
            $camelCapsPart = $functionName;
        } else {
            $packagePart   = substr($functionName, 0, $underscorePos);
            $camelCapsPart = substr($functionName, ($underscorePos + 1));

            // We don't care about _'s on the front.
            $packagePart = ltrim($packagePart, '_');
        }

        // If it has a package part, make sure the first letter is a capital.
        if ($packagePart !== '') {
            if ($functionName[0] === '_') {
                $error = 'Function name "%s" is invalid; only private methods '
                    . 'should be prefixed with an underscore';
                $phpcsFile->addError(
                    $error,
                    $stackPtr,
                    'FunctionUnderscore',
                    $errorData
                );
                return;
            }

            if ($functionName[0] !== strtoupper($functionName[0])) {
                $error = 'Function name "%s" is prefixed with a package name '
                    . 'but does not begin with a capital letter';
                $phpcsFile->addError(
                    $error,
                    $stackPtr,
                    'FunctionNoCaptial',
                    $errorData
                );
                return;
            }
        }

        // If it doesn't have a camel caps part, it's not valid.
        if (trim($camelCapsPart) === '') {
            $error = 'Function name "%s" is not valid; name appears incomplete';
            $phpcsFile->addError(
                $error,
                $stackPtr,
                'FunctionInvalid',
                $errorData
            );
            return;
        }

        $validName        = true;
        $newPackagePart   = $packagePart;
        $newCamelCapsPart = $camelCapsPart;

        $isCamelCaps = PHP_CodeSniffer::isCamelCaps(
            $camelCapsPart,
            false,
            true,
            false
        ) === false;
        
        // Every function must have a camel caps part, so check that first.
        if ($isCamelCaps) {
            $validName        = false;
            $newCamelCapsPart = strtolower($camelCapsPart[0])
                .substr($camelCapsPart, 1);
        }

        if ($packagePart !== '') {
            // Check that each new word starts with a capital.
            $nameBits = explode('_', $packagePart);
            foreach ($nameBits as $bit) {
                if ($bit[0] !== strtoupper($bit[0])) {
                    $newPackagePart = '';
                    foreach ($nameBits as $bit) {
                        $newPackagePart .= strtoupper($bit[0]).substr($bit, 1)
                            .'_';
                    }

                    $validName = false;
                    break;
                }
            }
        }

        if ($validName === false) {
            $newName = rtrim($newPackagePart, '_').'_'

                .$newCamelCapsPart;
            if ($newPackagePart === '') {
                $newName = $newCamelCapsPart;
            } else {
                $newName = rtrim($newPackagePart, '_').'_'.$newCamelCapsPart;
            }

            $error  = 'Function name "%s" is invalid; consider "%s" instead';
            $data   = $errorData;
            $data[] = $newName;
            $phpcsFile->addError(
                $error,
                $stackPtr,
                'FunctionNameInvalid',
                $data
            );
        }

    }//end processTokenOutsideScope() 
}
?>