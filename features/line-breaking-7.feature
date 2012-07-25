Feature: Line Breaking
  In order to have proper automatic line breaking

  Scenario: ERT Breaking test 5
    Given I am in buffer "Test_Breaking5.php"
    And I turn on php+-mode
    And I insert:
    """
    <?php
    function generate_streams_published_to_clause($username, $student_courses=null, $employee_courses=null)
    {
    }
    """
    And I break the statement at line 2
    Then I should see:
    """
    function generate_streams_published_to_clause(
        $username,
        $student_courses = null,
        $employee_courses = null
    ) {
    }
    """
