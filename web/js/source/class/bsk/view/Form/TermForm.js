/* ************************************************************************

    Класс описания формы создания и редактироанию директорий документов

************************************************************************ */


qx.Class.define("bsk.view.Form.TermForm",
{
    extend : bsk.view.Form.DocForm,

    construct : function(controller, Row) {
        this.base(arguments, controller, Row);
    },

    members : {

        /*
            Видимые поля формы,
                которые участвуют
                в обмене информацией.
        */
        fake_inp : {
            Type : null
        },
        
        term_types : {
            "7": 'Общий термин',
            "8": 'Воздушный термин',
            "9": 'Водный термин',
            "10": 'Наземный термин'
        },

        /**
            Строит визуальное представление формы
        **/

        buildForm : function() {
            var _base = this.base(arguments);
            var cnt =  _base.controller;
            var vertical_offset = _base.offset;
            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            
            this.fake_inp.Type = new qx.ui.form.SelectBox();
            this.fake_inp.Type.inp = this.inp;
            
            cnt.add(new qx.ui.basic.Label().set({value: "Тип" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.fake_inp.Type,   {row:vertical_offset, column:1});            
            this.addbuttonRow(cnt, ++vertical_offset);
            return cnt;
        },


        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = this.base(arguments);
            return flag;
        },

        /**
            Заполняет форму
        **/
        fillForm : function(data) {
            this.base(arguments, data);
            var _dir_id = this.inp.Dir_id.getValue()
            var defaultItem = null;
            for (var type in this.term_types)
            {
                var tempItem = new qx.ui.form.ListItem(this.term_types[type], null, type);
                if(!defaultItem)
                    defaultItem = tempItem;
                if(type == _dir_id)
                    defaultItem = tempItem;
                this.fake_inp.Type.add(tempItem);
            }
            this.fake_inp.Type.setSelection([defaultItem]);
            this.fake_inp .Type.addListener("changeSelection", function(e) {
                var diroffset = parseInt(e.getData()[0].getModel());
                var parent = parseInt(this.inp.Dir_id.getValue());
                var res = (diroffset);
                this.inp.Dir_id.setValue("" + res);
             });
        }
    }
});

