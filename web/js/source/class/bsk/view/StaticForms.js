
/* ************************************************************************

************************************************************************ */

qx.Class.define("bsk.view.StaticForms",
{
    extend : Object,

    statics : {

        //  форма редактирования xml игры
        gameXmlForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.GameXmlForm(biz, Row, formDescr);
        },

        // форма просмотра логов
        logForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.LogForm(biz, Row, formDescr);
        },

        // форма создания и редактирования рекламной компании
        advComForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.AdvComForm(biz, Row, formDescr);
        },

        // форма редкатирования карт
        gameEditorForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.GameEditorForm(biz, Row, formDescr);
        },

        /// Форма создания и редактирования групп пользователей
        customerGroupForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.CustomerGroupForm(biz, Row, formDescr);
        },

        /// Форма создания и редактирования пользователей
        customerForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.CustomerForm(biz, Row, formDescr);
        },

        /// Форма создания и редактирования документов
        docForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.DocForm(biz, Row, formDescr);
        },

        dirForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.DirForm(biz, Row, formDescr);
        },

        termForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.TermForm(biz, Row, formDescr);
        },
        
        testForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.TestForm(biz, Row, formDescr);
        },
        
        testQuestionForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.TestQuestionForm(biz, Row, formDescr);
        },
        
        testAnswerForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.TestAnswerForm(biz, Row, formDescr);
        },

        
        confForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.ConfForm(biz, Row, formDescr);
        },
        
        confQuestionForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.ConfQuestionForm(biz, Row, formDescr);
        },
        
        confAnswerForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.ConfAnswerForm(biz, Row, formDescr);
        }
    }
});

