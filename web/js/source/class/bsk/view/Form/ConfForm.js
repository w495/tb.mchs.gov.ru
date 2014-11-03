/* ************************************************************************

    Класс описания формы создания и редактироанию директорий документов

************************************************************************ */


qx.Class.define("bsk.view.Form.ConfForm",
{
    extend : bsk.view.Form.DirForm,

    construct : function(controller, Row) {
        this.base(arguments, controller, Row);
    },
    
    members : {
        
        /*
            Поля формы видимые и невидимые,
            которые участвуют в обмене информацией.
        */
        inp2 : {
            Start           : null,
            Stop            : null
        },
                
        urc : {  // upload request config
            url: "/update-conf",
            imgurl: "/update-doc/upload-image",
            method: "POST",
            mimetype: "application/json"
        },
        
        drc : {  // download request config
            url: "/get-conf-info",
            method: "GET",
            mimetype: "application/json"
        },
        
        /* Cписок экспертов */
        expertList: null,
        expertListOptions: {
            url:            "/get-experts",
            labelFieldName: "firstname",
            descrFieldName: "lastname"
        },
        
        buildForm : function() {
            var _base = this.base(arguments);
            var cnt =  _base.controller;
            var vertical_offset = _base.offset;
            
            if(this.inp.Content){
                this.inp.Content.setWidth(200 );
                this.inp.Content.setHeight(bsk.Config.TEST_FORM_HEIGHT);
            }
            
            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            
            this.expertList = new bsk.view.SelListTree(this,
                this.expertListOptions.url,
                this.expertListOptions.labelFieldName,
                this.expertListOptions.descrFieldName
            );
            
            // ----------------------------------------------------------
            // Дата
            // ----------------------------------------------------------
            
            var  df = new qx.util.format.DateFormat("YYYY.MM.dd", 'ru')
            
            this.inp2 .Start= new qx.ui.form.DateField();
            this.inp2 .Start.setDateFormat(df);
            cnt.add(new qx.ui.basic.Label().set({value: "Начало",  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp2 .Start,   {row:vertical_offset, column:1});
            this.inp2 .Start.setValue(new Date());
            
            this.inp2 .Stop = new qx.ui.form.DateField();            
            this.inp2 .Stop.setDateFormat(df);
            cnt.add(new qx.ui.basic.Label().set({value: "Конец",  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp2 .Stop,   {row:vertical_offset, column:1});
            this.inp2 .Stop.setValue(new Date());
            
            // ----------------------------------------------------------
            
            this.expertList.setWidth(300);
            var l2 = new qx.ui.basic.Label("Эксперты");
            l2.setFont("bold");
            cnt.add(l2, {row:0, column:2});
            cnt.add(this.expertList , {row:1, column:2, rowSpan: vertical_offset});
            
            
            var tabModel = {
                type : "partial-table",
                columns : [
                    {name : "id", alias : "#", type : "float", sortable : true, width:"5%"},
                    {name : "datatime", alias : "Дата", type : "erl_datetime_utc", sortable : true},
                    {name : "name", alias : "Имя", type : "string", sortable : true},
                    {name : "published", alias : "Обубликован", type : "string", sortable : true}
                ],
                filter :    { submit_url : "/get-conf-questions" },
                sort :      "datatime",
                ascending : false,                
                dblclick_action : "resource/bsk/descr/conf-question-form.json",
                add_action: "resource/bsk/descr/conf-question-form.json",
                edt_action: "resource/bsk/descr/conf-question-form.json",
                app_action: "/approve-conf-question",
                rem_action: "/delete-conf-question"
            };

            
            this.questionTab = new bsk.view.Form.ConfQuestionTabContainer(this.controller.biz, tabModel);
                
            if(!this.createNew){
                /* TODO: костыль,пока не можем создавать вопросы
                    для новых тестов */
                cnt.add(new qx.ui.basic.Label().set({value: "Вопросы",  rich : true}),
                        {row:++vertical_offset, column:0});
                cnt.add(this.questionTab,   {row:vertical_offset, column:1, colSpan:3});
            }
            
            this.addbuttonRow(cnt, ++vertical_offset);
            return cnt;
        },
        
        /**
            Формирует данные для сервера
        **/
        _uploadData : function(e) {
            this.base(arguments, e);
            var expertIdList = this.expertList.getSelectedId();
            
            var Start = bsk.util.utils.normalize_date(this.inp2.Start.getValue());
            var Stop = bsk.util.utils.normalize_date(this.inp2.Stop.getValue());
            
            if(this.validateForm()) {
                this.uReq.setParameter("experts", expertIdList, true);
                this.uReq.setParameter("start", Start, true);
                this.uReq.setParameter("stop", Stop, true);
            }
        },
        
        fillForm : function(data) {

                
            this.base(arguments, data);
            var _dir_id = this.inp.Parent_dir_id.getValue()
            var defaultItem = null;
            
            if(!this.createNew){
                this.questionTab.tabModel.vardata = {id : data.value['id']};
                this.questionTab.refresh();
            }
            else{
                //this.startDateField.setValue(new Date());
                //this.endDateField.setValue(new Date());
            }
            
            if(data.value['start'])
                this.inp2 .Start.setValue(new Date(data.value['start']));
            if(data.value['stop'])
                this.inp2 .Stop.setValue(new Date(data.value['stop']));
            
            if(data.experts)
                this.expertList.setChecked(data.experts);
        }
    }
});

