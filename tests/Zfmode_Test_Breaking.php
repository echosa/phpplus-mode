<?php
/**
 * zf-mode Test Class
 *
 * PHP Version 5.3
 *
 * @category   ZfMode
 * @package    Test
 * @subpackage Class
 *
 * @author     Brian Zwahr <bzwahr@tamu.edu>
 * @copyright  2011 Texas A&M University
 * @license    http://www.gnu.org/licenses/gpl-3.0.html  GPLv3
 * @version    SVN: $Id: $
 * @link       http://www.tamu.edu
 * @see        Zfmode_Test
 */

/**
 * zf-mode Test Class
 *
 * @category   ZfMode
 * @package    Test
 * @subpackage Class
 *
 * @author     Brian Zwahr <bzwahr@tamu.edu>
 * @license    http://www.gnu.org/licenses/gpl-3.0.html  GPLv3
 * @link       http://www.tamu.edu
 * @see        Zfmode_Test.php
 */
class Zfmode_Test_Breaking
{
    /**
     * First test method.
     *
     * First test method. Should be public.
     *
     * @author Brian Zwahr <bzwahr@tamu.edu>
     *
     * @return void
     */
    public function method1()
    {
        $person = new Occhealth_Model_Personnel(
            $this->getRequest()->getParams()
        );
    }

    /**
     * Second test method.
     *
     * Second test method. Should be public.
     *
     * @author Brian Zwahr <bzwahr@tamu.edu>
     *
     * @return void
     */
    public function method2()
    {
        $this->someCall('someString');

        $select = $this->select()->where('person = ?', $person->pk)
            ->order('date DESC');
    }

    /**
     * test method
     *
     * @author Brian Zwahr <bzwahr@tamu.edu>
     *
     * @return void
     */
    private function _method3()
    {
        $string = 'Is Certified: '
            . $this->view->yesOrNo($location->isCertified)
            . '<br />Date Certified: ';
    }
}
