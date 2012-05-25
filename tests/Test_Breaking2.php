<?php
/**
 * OccHealth Personnel Controller
 *
 * OccHealth Personnel Controller
 *
 * PHP Version 5.3
 *
 * @category   WebApp
 * @package    Biosafety
 * @subpackage OccHealth
 *
 * @author     Brian Zwahr <bzwahr@tamu.edu>
 * @copyright  2011 Texas A&M University
 * @license    http://www.gnu.org/licenses/gpl-3.0.html  GPLv3
 * @version    SVN: $Id: $
 * @link       http://www.tamu.edu
 */

/**
 * OccHealth Personnel Controller
 *
 * OccHealth Personnel Controller
 *
 * @category   WebApp
 * @package    Biosafety
 * @subpackage OccHealth
 *
 * @author     Brian Zwahr <bzwahr@tamu.edu>
 * @license    http://www.gnu.org/licenses/gpl-3.0.html  GPLv3
 * @link       http://www.tamu.edu
 */
class Occhealth_PersonnelController extends Zend_Controller_Action
{
    /**
     * Add personnel to the OccHealth system
     *
     * Add personnel to the OccHealth system
     *
     * @author Brian Zwahr <bzwahr@tamu.edu>
     *
     * @return void
     */
    public function addAction()
    {
        $this->view->form = new Occhealth_Form_Personnel();
        if ($this->getRequest()->isPost()) {
            if ($this->view->form->isValid($this->getRequest()->getParams())) {
                $person = new Occhealth_Model_Personnel(
                    $this->getRequest()->getParams()
                );
                $person->save();
                $this->_helper->flashMessenger(
                    $person->getName() . ' added to Occupation Health.'
                );
            }
        }
    }

}
