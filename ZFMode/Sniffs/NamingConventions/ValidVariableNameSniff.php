<?php
class ZFMode_Sniffs_NamingConventions_ValidVariableNameSniff
    extends PEAR_Sniffs_NamingConventions_ValidVariableNameSniff
{
    /**
     * Processes class member variables.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token
     *                                        in the stack passed in $tokens.
     *
     * @return void
     */
    protected function processMemberVar(PHP_CodeSniffer_File $phpcsFile,
        $stackPtr
    ) {
        $tokens = $phpcsFile->getTokens();

        $memberProps = $phpcsFile->getMemberProperties($stackPtr);
        if (empty($memberProps) === true) {
            return;
        }

        $memberName     = ltrim($tokens[$stackPtr]['content'], '$');
        $isPublic       = ($memberProps['scope'] === 'public');
        $isPrivate       = ($memberProps['scope'] === 'private');
        $scope          = $memberProps['scope'];
        $scopeSpecified = $memberProps['scope_specified'];

        // If it's a private member, it must have an underscore on the front.
        if ($isPrivate === true && $memberName{0} !== '_') {
            $error = 'Private member variable "%s" must be prefixed with an ' .
                'underscore';
            $data  = array($memberName);
            $phpcsFile->addError(
                $error,
                $stackPtr,
                'PrivateNoUnderscore',
                $data
            );
            return;
        }

        // If it's a public member, it must not have an underscore on the front.
        if ($isPublic === true && $scopeSpecified === true
            && $memberName{0} === '_'
        ) {
            $error = 'Public member variable "%s" must not be prefixed with ' .
                'an underscore';
            $data  = array($memberName);
            $phpcsFile->addError($error, $stackPtr, 'PublicUnderscore', $data);
            return;
        }

    }
}
?>