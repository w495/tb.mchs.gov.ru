/* ************************************************************************

    Класс абстрактной формы 

************************************************************************ */



qx.Class.define("bsk.view.Form.AbstractForm",
{
    type : "abstract",

    extend : qx.core.Object,

    construct : function(controller) {
        this.controller = controller;
        this.form = new qx.ui.form.Form();

        this.submitButton =  new qx.ui.form.Button("Отправить");
        this.cancelButton = new qx.ui.form.Button("Отмена");
    },

    statics : {

        /* Константы */

        REQUIRED_FIELD_MARKER   : "&nbsp;<span style='color:red;'>*</span>",

        /* Статичные методы */

        password : function(fName, form, item) {
            item.fName = fName;
            item.form = form;
            return bsk.view.Form.AbstractForm.checkPassword;
        },

        checkPassword : function(value, item) {
            var i = item.form.getItems();
            var secondVal = i[item.fName].getValue();
            if(secondVal != value)
                throw new qx.core.ValidationError("Validation Error: ", "Пароли не совпадают");
        },

        num : function() {
            return bsk.view.Form.AbstractForm.checkRequired;
        },

        checkRequired : function(value, item) {
            var tmp = "" + value;
            if(item.getRequired() && (tmp.length == 0 || value == null || value == undefined))
                throw new qx.core.ValidationError("Validation Error: ", "Это поле должно быть заполнено");
        },

        checkNumber : function(value, item) {
            bsk.view.Form.AbstractForm.checkRequired(value, item);
            if ((typeof value !== "number" && (!(value instanceof Number))) && (!(isFinite(value)))) {
                throw new qx.core.ValidationError("Validation Error: ", value + " не число.");
            }
        },

        checkStringLength : function(from, to, field) {
            field.minValueLength = from;
            field.maxValueLength = to;
            return bsk.view.Form.AbstractForm._checkStrLength;
        },

        _checkStrLength : function(value, item) {
            bsk.view.Form.AbstractForm.checkRequired(value, item);
            if(value == undefined || value.length == undefined || value.length < item.minValueLength || value.length > item.maxValueLength) {
                throw new qx.core.ValidationError("Validation Error: ", "Значение должно быть не менее " + item.minValueLength + " и не более " + item.maxValueLength + " символов");
            }
        },

        checkStringMax : function(max, field) {
            field.maxStrLength = max;
            return bsk.view.Form.AbstractForm._checkStrMax;
        },

        _checkStrMax : function(value, item) {
            bsk.view.Form.AbstractForm.checkRequired(value, item);
            if(value != undefined && value.length != undefined && value.length > item.maxStrLength) {
                throw new qx.core.ValidationError("Validation Error: ", "Значение должно занимать не более " + item.maxStrLength + " символов");
            }
        },

        customFormChkMaxLength : function(max, field) {
            var value = field.getValue();
            if(value != undefined && value.length != undefined && value.length > max) {
                field.setValid(false);
                field.setInvalidMessage("Значение должно занимать не более " + max + " символов");
                return false;
            }
            return true;
        },

        /**
            Проверка длинны введенного текста в поле.
        **/
        customFormChkLength : function(from, to, field) {
            var value = field.getValue();
            if(value == undefined || value.length == undefined || value.length < from || value.length > to) {
                field.setValid(false);
                field.setInvalidMessage("Значение должно быть не менее " + from + " и не более " + to + " символов");
                return false;
            }
            return true;
        },

        customFormPassCheck : function(pass1, pass2) {
            if(pass1.getValue() != pass2.getValue()) {
                pass1.setValid(false);
                pass2.setValid(false);
                pass1.setInvalidMessage("Пароли не совпадают");
                pass2.setInvalidMessage("Пароли не совпадают");
                return false;
            }
            return true;
        },

        customFormcheckNumber : function(field) {
            var value = field.getValue();
            if (value  && ((typeof value !== "number" && (!(value instanceof Number))) && (!(isFinite(value))))) {
                field.setValid(false);
                field.setInvalidMessage("Должно быть число");
                return false;
            }
            return true;
        },


        customFormCheckRequired : function(field) {
            var value = field.getValue();
            if(value == undefined || value.length == undefined || value.length == 0) {
                field.setValid(false);
                field.setInvalidMessage("Поле обязательно для заполнения");
                return false;
            }
            return true;
        }

    },
 
    members : {

        /* Кнопки */
        submitButton: null, // new qx.ui.form.Button("Отправить"),
        cancelButton: null, // new qx.ui.form.Button("Отмена"),

        /**
            Выводит сообщение об ошибке для данного поля
        **/
        showEMsg : function(fieldName, msg) {
            var fFields = this.form.getItems();
            if(fFields[fieldName] != undefined) {
                fFields[fieldName].setInvalidMessage(msg);
                fFields[fieldName].setValid(false);
                alert("fieldName = " + fieldName);
            }
            else {
                alert("Ошибка сервера - " + msg + " для " + fieldName);
            }
        },
        
        setEnabled: function(bool) {
            console.log(">>>>>>>>>>>>>>> setEnabled = ", bool);
            console.log(">>>>>>>>>>>>>>> this.form = ", this.form);
            console.log(">>>>>>>>>>>>>>> this.form.getItems() = ", this.form.getItems());
            var fFields = this.form.getItems();
            for(var field in fFields){
                field.setEnabled(bool);
                console.log("field", field);
            }            
        },

        submit : function(req) {
            req.addListener("completed", this._onSubmitCompleted, this);
            req.send();
        },

        // @depricated
        _onSubmitCompleted : function(response) {
            this.controller.enableForm();
            var result = response.getContent();
            if(result.ERROR != undefined) {
                switch(result.ERROR.type) {
                    case "unknown":
                        alert("Ошибка сервера: " + result.ERROR.info);
                        break;
                    case "not_null":
                        this.showEMsg(result.ERROR.info, "поле не заполнено");
                        break;
                    case "not_unique":
                        this.showEMsg(result.ERROR.info, "поле должно быть уникально");
                        break;
                    default:
                        alert("Неизвестная ошибка сервера: " + result.ERROR.type);
                        break;
                }
            }
            else {
                this.onFormClose();
                this.controller.submited(result);
            }
        },

        /* Виртуальная фукция посылки на сервер */
        _onSubmitClick : function() {},

        /* Виртуальная отмены */
        _onCancelClick : function() {},

        onFormClose : function() {},

        show_error : function(etype, emsg) {
            alert("Ошибка (" + etype + "): " + emsg);
        }
    }
});

