/* ************************************************************************

    Класс описания формы создания и редактироанию директорий

************************************************************************ */


qx.Class.define("bsk.view.Form.DirForm",
{
    extend : bsk.view.Form.DDForm,

    construct : function(controller, Row) {        
        this.base(arguments, controller, Row);
        this.addListeners();
    },

    members : {

        /* Редактируем или создаем новое */
        createNew : false,

        urc : {  // upload request config
            url: "/update-dir",
            imgurl: "/update-doc/upload-image",
            method: "POST",
            mimetype: "application/json"
        },

        drc : {  // download request config
            url: "/get-dir-info",
            method: "GET",
            mimetype: "application/json"
        },
        
        inp : {
            Id                         : null,
            Name                       : null,
            Pic_url                    : null,
            Content                    : null,
            Dir_type_id                : null,
            Doc_description_id         : null
        },

        drc_create : {  // download request config
            /**
                В таком варианте используется для создания новой папки.
                Для создания папки нам надо как-то
                заранее узнать к какой подпапке она относится.
                В общем случае, можно было бы получать данные по простому drc,
                но в этом случае, мы бы тянули лишние данные с сервера.
            **/
            url: "/get-dir-parent-info",
            method: "GET",
            mimetype: "application/json"
        },
        
        testCreate: function(Row) {
            this.createNew = (Row.isNew == true);
            return this.createNew;
        },
        
        /**
            Строит визуальное представление формы
        **/
        buildForm : function() {
            this.inp.Id                   = new qx.ui.form.TextField();
            this.inp.Name                 = new qx.ui.form.TextField();
            this.inp.Pic_url                = new qx.ui.form.TextField(); /* hidden */
            this.inp.Parent_dir_id        = new qx.ui.form.TextField(); /* hidden */
            this.inp.Dir_type_id          = new qx.ui.form.TextField(); /* hidden */
            this.inp.Doc_description_id   = new qx.ui.form.TextField(); /* hidden */
            /* Текстовое описание директории */
            this.inp.Content         = new bsk.view.Form.Format.RichTextArea();
            
            var cnt = this.base(arguments);
            var vertical_offset = 0;
            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            cnt.add(new qx.ui.basic.Label("#"),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Id, {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Название" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Name,      {row:vertical_offset , column:1});

            cnt.add(new qx.ui.basic.Label().set({value: "Картинка:" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this._buildPicFormCnt(), {row:vertical_offset , column:1});
            
            
            cnt.add(new qx.ui.basic.Label().set({value: "Содержание" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Content,   {row:vertical_offset , column:1});
            
            if(!this.createNew){
                /* TODO: костыль,пока не можем создавать аттачи для новых */
                cnt.add(new qx.ui.basic.Label().set({value: "Вложения:",  rich : true}),
                        {row:++vertical_offset, column:0});
                cnt.add(this._buildAttachFormCnt(),{row:vertical_offset, column:1});
            }
            
            this.addbuttonRow(cnt, ++vertical_offset);
            this.controller.placeForm(cnt);
            this.inp.Name.focus();
            return {controller : cnt, offset: vertical_offset};
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
            if(this.createNew){
                this.inp["Dir_type_id"].setValue(data.value["dir_type_id"])
                this.inp["Parent_dir_id"].setValue(data.value["id"])
            }
            else{
                for(var fieldName in this.inp){
                    var item = fieldName.toLowerCase();
                    var value = data.value[item];
                    
                    if ("name" == item || "content" == item)
                        value = bsk.util.utils.unnormalize(value);
                        
                    this.inp[fieldName].setValue(value)
                }                    
                var _id = data.value["doc_description_id"];
                this.picForm.setParameter("id", _id);
                this.attachFormCnt.setId(_id);
                if(data.attaches){
                    console.log("data.attaches", data.attaches);
                    this.attachList.addItems(data.attaches);
                }
            }
        }
    }
});

