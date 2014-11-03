/* ************************************************************************

    Класс описания формы создания и редактироанию директорий документов

************************************************************************ */


qx.Class.define("bsk.view.Form.TestForm",
{
    extend : bsk.view.Form.DirForm,

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
            "15":  'Воздушный тест',
            "16":  'Водный тест',
            "17": 'Наземный тест'
        },

        /**
            Строит визуальное представление формы
        **/
        buildForm : function() {
            var _base = this.base(arguments);
            var cnt =  _base.controller;
            var vertical_offset = _base.offset;
            if(this.inp.Content){
                this.inp.Content.setWidth(bsk.Config.TEST_FORM_WIDTH );
                this.inp.Content.setHeight(bsk.Config.TEST_FORM_HEIGHT);
            }
            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            this.fake_inp.Type = new qx.ui.form.SelectBox();
            this.fake_inp.Type.inp = this.inp;
            
            cnt.add(new qx.ui.basic.Label().set({value: "Тип" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.fake_inp.Type,   {row:vertical_offset, column:1});
            
            var tabModel = {
                type : "partial-table",
                columns : [
                    {name : "id", alias : "#", type : "float", sortable : true, width:"5%"},
                    {name : "datatime", alias : "Дата", type : "erl_datetime_utc", sortable : true},
                    {name : "name", alias : "Имя", type : "string", sortable : true}
                ],
                filter :    { submit_url : "/get-dirs" },
                sort :      "datatime",
                ascending : false,                
                dblclick_action : "resource/bsk/descr/test-question-form.json",
                add_action: "resource/bsk/descr/test-question-form.json",
                edt_action: "resource/bsk/descr/test-question-form.json",
                rem_action: "/delete-dir"
            };
            this.questionTab = new bsk.view.Form.TestQuestionTabContainer
                (this.controller.biz, tabModel);
            if(!this.createNew){
                /* TODO: костыль,пока не можем создавать вопросы
                    для новых тестов */
                cnt.add(new qx.ui.basic.Label().set({value: "Вопросы",  rich : true}),
                        {row:++vertical_offset, column:0});
                cnt.add(this.questionTab,   {row:vertical_offset, column:1});            
            }
            this.addbuttonRow(cnt, ++vertical_offset);
            return cnt;
        },

        /**
            Заполняет форму
        **/
        fillForm : function(data) {
            this.base(arguments, data);
            var _dir_id = this.inp.Parent_dir_id.getValue()
            var defaultItem = null;
            for (var type in this.term_types)
            {
                var tempItem = new qx.ui.form.ListItem(this.term_types[type],
                                                        null, type);
                if(!defaultItem){
                    defaultItem = tempItem;
                    // У терминов этого нет.
                    this.inp.Parent_dir_id.setValue(type);
                }
                if(type == _dir_id){
                    defaultItem = tempItem;
                    // У терминов этого нет.
                    this.inp.Parent_dir_id.setValue(type);
                }
                this.fake_inp.Type.add(tempItem);
            }
            this.fake_inp.Type.setSelection([defaultItem]);
            this.fake_inp .Type.addListener("changeSelection", function(e) {
                var diroffset = parseInt(e.getData()[0].getModel());
                var parent = parseInt(this.inp.Parent_dir_id.getValue());
                var res = (diroffset);
                this.inp.Parent_dir_id.setValue("" + res);
             });
            if(!this.createNew){
                this.questionTab.tabModel.vardata = {id : data.value['id']};
                this.questionTab.refresh();
            }
        }
    }
});

