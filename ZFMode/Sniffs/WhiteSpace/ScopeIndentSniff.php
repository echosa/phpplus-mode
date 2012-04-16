<?php
class ZFMode_Sniffs_WhiteSpace_ScopeIndentSniff
    extends PEAR_Sniffs_WhiteSpace_ScopeIndentSniff
{
    protected function calculateExpectedIndent(array $tokens, $stackPtr)
    {
        $conditionStack = array();
                
        // Empty conditions array (top level structure).
        if (empty($tokens[$stackPtr]['conditions']) === true) {
            return 1;
        }

        $tokenConditions = $tokens[$stackPtr]['conditions'];
        foreach ($tokenConditions as $id => $condition) {
            // If it's an indenting scope ie. it's not in our array of
            // scopes that don't indent, add it to our condition stack.
            if (in_array($condition, $this->nonIndentingScopes) === false) {
                $conditionStack[$id] = $condition;
            }
        }

        // Handle inline closures 
        if ($tokens[$stackPtr]['code'] == T_CLOSURE) {
            $parenthesisStack = $tokens[$stackPtr]['nested_parenthesis'];
            return (((count($conditionStack) + count($parenthesisStack))
                    * $this->indent) + 1);
        }
        
        return ((count($conditionStack) * $this->indent) + 1);
    }
}
?>