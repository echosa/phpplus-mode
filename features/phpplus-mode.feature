Feature: Statement Cont Lineup
  In order to line up a statement continuation
  As a user
  I want to press the tab key
  
  Scenario: Second line of anonymous function
    Given I am in buffer "test.php"
    And I insert: 
    """
    <?php
    class EmployeeData
    {
        protected function escape($string)
        {
            join(
                '',
                array_map(
                    split('', $string),
                    function ($char) {
                        return (in_array($char, array('\\', '\'')) ? '\\' : '')
                . $char;
                    }
                )
            );
        }
    }
    """
    And I turn on php+-mode
    And I go to line "12"
    And I press "TAB"
    Then the cursor should be before ". $char;"
    And I should be at column 24

Feature: Font Locking
  In order to see properly font locked text
  As a user
  I want to see keywords highlighted properly

  Scenario: Font locking of anonymous function and return
    Given I am in buffer "test.php"
    And I turn on font-lock-fontify-buffer
    And I go to the beginning of text on line 10
    Then I should see the face font-lock-keyword-face
    And I go to the beginning of text on line 11
    Then I should see the face font-lock-keyword-face

