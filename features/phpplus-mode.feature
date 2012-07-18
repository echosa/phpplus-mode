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
