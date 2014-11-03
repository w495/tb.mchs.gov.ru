/* ************************************************************************

#asset(qx/icon/Tango/16/actions/document-save.png)

************************************************************************ */

/**
 *     Класс описания формы создания и редактироанию директорий документов
 *
 */

qx.Class.define("bsk.view.Form.ConfAnswerForm",
{
    extend : bsk.view.Form.DDForm,

    construct : function(controller, Row) {
        this.base(arguments, controller, Row);
        this.addListeners();
    },

    members : {
        
        /*
            Поля формы видимые и невидимые,
            которые участвуют в обмене информацией.
        */
        inp : {
            Id              : null,
            Name            : null,
            Content         : null,
            Published       : null,
            Pic_url         : null,
            Correct_flag    : null,
            Dir_id          : null, /* hidden */
            Doc_type_id     : null  /* hidden */
        },

        urc : {  // upload request config
            url: "/update-doc",
            imgurl: "/update-answer/upload-image",
            method: "POST",
            mimetype: "application/json"
        },

        drc : {  // download request config
            url: "/get-doc-info",
            method: "GET",
            mimetype: "application/json"
        },

        /**
            Строит визуальное представление формы
        **/
        buildForm : function() {
            this.inp.Id              = new qx.ui.form.TextField();
            this.inp.Name            = new qx.ui.form.TextField();
            this.inp.Content         = new bsk.view.Form.Format.RichTextArea();
            this.inp.Published       = new qx.ui.form.CheckBox("");
            this.inp.Correct_flag    = new qx.ui.form.CheckBox("");
            this.inp.Pic_url         = new qx.ui.form.TextField();
            this.inp.Dir_id          = new qx.ui.form.TextField(); /* hidden */
            this.inp.Doc_type_id     = new qx.ui.form.TextField();  /* hidden */
            
            var cnt = this.base(arguments);
            var vertical_offset = 0;
            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            cnt.add(new qx.ui.basic.Label("#"), {row:++vertical_offset, column:0});
            cnt.add(this.inp.Id, {row:vertical_offset , column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "Название:" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Name, {row: vertical_offset , column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "Картинка:" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this._buildPicFormCnt(), {row:vertical_offset , column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "Содержание:" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Content, {row:vertical_offset , column:1});
            
            if(!this.createNew){
                /* TODO: костыль,пока не можем создавать аттачи для новых */
                cnt.add(new qx.ui.basic.Label().set({value: "Вложения:",  rich : true}),
                        {row:++vertical_offset, column:0});
                cnt.add(this._buildAttachFormCnt(),{row:vertical_offset, column:1});
            }
            
            cnt.add(new qx.ui.basic.Label().set({value: "Опубликован" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Published, {row:vertical_offset , column:1});

            
            this.addbuttonRow(cnt, ++vertical_offset);
            return {controller : cnt, offset: vertical_offset};
        },

        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = this.base(arguments);
            return flag;
        },

        _uploadData : function(e) {
            this._dropInvalid();
            var res = {}
            for(var fieldName in this.inp){
                item = fieldName.toLowerCase()
                res[item] = this.inp[fieldName].getValue();
                for(var i in ["name", "content"])
                    res[i] = bsk.util.utils.normalize(res[i])
                if("published" == item ){
                    res[item] = this.inp[fieldName].getValue() ? "true" : "false";
                    continue;
                }
                if("correct_flag"  == item ){
                    res[item] = this.inp[fieldName].getValue() ? "true" : "false";
                    continue;
                }
            }
            if(this.validateForm()) {
                this.uReq = undefined;
                this.uReq = new qx.io.remote.Request(this.urc.url, this.urc.method, this.urc.mimetype);
                for(var item in res){
                    this.uReq.setParameter(item, res[item], true);
                }
            }
        },

        /**
            Заполняет форму
        **/
        fillForm : function(data) {
            if(this.createNew){
                this.inp["Dir_id"].setValue(data.value["id"]);
                this.inp["Doc_type_id"].setValue("2");
                this.inp["Published"].setValue(true);
                // this.attachFormCnt.hide();
            }
            else{
                for(var fieldName in this.inp){
                    var item = fieldName.toLowerCase();
                    var value = data.value[item];
                    if("published" == item ){
                        this.inp[fieldName].setValue(/^true$/i.test(value));
                        continue;
                    }
                    if("correct_flag" == item ){
                        this.inp[fieldName].setValue(/^true$/i.test(value));
                        continue;
                    }
                    console.log("fieldName = ", fieldName);
                    
                    this.inp[fieldName].setValue(value);
                }
                if(data.attaches){
                    // console.log("data.attaches", data.attaches);
                    
                    var _id = data.value["id"];                    
                    this.picForm.setParameter("id", _id);
                    this.attachFormCnt.setId(_id);
                    this.attachList.addItems(data.attaches);
                }
            }
        }
    }
});

