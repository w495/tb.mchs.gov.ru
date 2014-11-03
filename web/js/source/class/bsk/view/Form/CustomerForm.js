/* ************************************************************************

    Класс описания формы по созданию пользователя
    ИСПОЛЬЗОВАНИЕ:
        Администрировнаие > Пользователи >  [Создать] | [Редактировать]

************************************************************************ */


qx.Class.define("bsk.view.Form.CustomerForm",
{
    extend : bsk.view.Form.BaseForm,

    construct : function(controller, Row) {
        this.base(arguments, controller, Row);
        this.addListeners();
    },

    members : {

        urc : {  // upload request config
            url: "/update-customer",
            method: "POST",
            imgurl: "/update-customer/upload-image",
            mimetype: "application/json"
        },

        drc : {  // download request config
            url: "/get-customer-info",
            method: "GET",
            mimetype: "application/json"
        },
        
        /* Поля формы */
        inp : {
            Id              : null,
            Login           : null,
            Password1       : null,
            Password2       : null,
            Email           : null,
            City            : null,
            Organization    : null,
            Position        : null,
            Firstname       : null,
            Lastname        : null,
            Patronimic      : null,
            Description     : null,
            Pic_url         : null
        },
        
        /* Cписок групп справа */
        groupList: null,
        groupListOptions: {
            url:            "/get-customer-groups",
            labelFieldName: "name",
            descrFieldName: "description"
        },

        /**
            Строит визуальное представление формы
            TODO: отрефакторить, так чтобы было мало букаф
        **/
        
        /* Виджеты для сопровождающей картикни */
        picForm: null,
        picButton: null,
        

        buildForm : function() {
            this.base(arguments);

            this.inp.Id           = new qx.ui.form.TextField();
            this.inp.Login        = new qx.ui.form.TextField();
            this.inp.Password1    = new qx.ui.form.PasswordField();
            this.inp.Password2    = new qx.ui.form.PasswordField();
            this.inp.Email        = new qx.ui.form.TextField();
            this.inp.City         = new qx.ui.form.TextField();
            this.inp.Organization = new qx.ui.form.TextField();
            this.inp.Position     = new qx.ui.form.TextField();
            this.inp.Firstname    = new qx.ui.form.TextField();
            this.inp.Lastname     = new qx.ui.form.TextField();
            this.inp.Patronimic   = new qx.ui.form.TextField();
            this.inp.Pic_url      = new qx.ui.form.TextField();
            
            this.groupList = new bsk.view.SelListTree(this,
                this.groupListOptions.url,
                this.groupListOptions.labelFieldName,
                this.groupListOptions.descrFieldName
            );
        
            /* Сопровождающая картинка */
            this.picButton = new bsk.view.Form.Upload.UploadButton("uploadfile", null, "icon/16/actions/document-save.png"),
            this.picForm = new bsk.view.Form.Upload.UploadForm('uploadFrm', this.urc.imgurl);

            var layout = new qx.ui.layout.Grid(12, 6);
            var cnt = new qx.ui.container.Composite(layout);

            this.inp.Id.setEnabled(false);
            this.groupList.setWidth(300);

            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");

            var l1 = new qx.ui.basic.Label("Общая информация");
            l1.setFont("bold");
            l1.setAlignX("left");
            cnt.add(l1, {row:0, column:0, colSpan:2});

            var vertical_offset = 0;
            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;

            cnt.add(new qx.ui.basic.Label("#"),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Id,         {row:vertical_offset , column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "Логин"             + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Login,      {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Пароль"            + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Password1,  {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Повторите пароль"  + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Password2,  {row:vertical_offset , column:1});

            cnt.add(new qx.ui.basic.Label().set({value: "Фотография:" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this._buildPicFormCnt(), {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "E-mail"            + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Email,          {row:vertical_offset , column:1});

            cnt.add(new qx.ui.basic.Label().set({value: "Имя"               + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Firstname,  {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Отчество"          + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Patronimic, {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Фамилия"           + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Lastname,   {row:vertical_offset , column:1});

            
            cnt.add(new qx.ui.basic.Label().set({value: "Город",                    rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.City,           {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Организация",              rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Organization,   {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Должность",                rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Position,       {row:vertical_offset , column:1});

            
            var l2 = new qx.ui.basic.Label("Группы");
            l2.setFont("bold");
            cnt.add(l2, {row:0, column:2});
            cnt.add(this.groupList, {row:1, column:2, rowSpan: vertical_offset});

            this.addbuttonRow(cnt, ++vertical_offset);

            this.controller.placeForm(cnt);
            this.inp.Login.focus();
        },
        
        /**
            Обработчики событий,
                которые не удалось вынести внутрь
                    отдельных виджетов.
        **/
        addListeners: function() {            
            var _this = this;
            
            /* События виджетов для сопровождающей картикни  */
            this.picButton.addListener('changeFileName',function(e){
                if('' != e.getData()) {
                    bsk.view.Form.Upload.UploadFakeStatusBar.on();
                    _this.picForm.setParameter("prev", _this.inp.Pic_url.getValue());
                    _this.inp.Pic_url.setValue(_this.picButton.getFileName());
                    _this.picForm.send();    
                }
            });
            
            this.picForm.addListener('completed',function(e) {
                var response = _this.picForm.getIframeTextContent();
                bsk.view.Form.Upload.UploadFakeStatusBar.off();
                _this.inp.Pic_url.setValue(response);
            });  
        },
        
        /**
            Создает область загрузки картинки.
        **/
        _buildPicFormCnt: function() {
            var pic_layout = new qx.ui.layout.Grid(12, 6);
            var picFormCnt = new qx.ui.container.Composite(pic_layout).set({
                allowGrowX: true
              });
            if(!this.inp.Pic_url)
                return picFormCnt;
            
            pic_layout.setColumnFlex(0, 1);
            pic_layout.setColumnAlign(0, "right", "middle");
            picFormCnt.add(this.inp.Pic_url,  {row:0, column:0});
            this.picForm.setParameter('rm','upload');
            this.picForm.setLayout(new qx.ui.layout.Basic);
            picFormCnt.add(this.picForm, {row:0, column:1});
            this.picForm.add(this.picButton , {left:0,top:0});
            return picFormCnt;
        },
        
        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var pass1 = this.inp.Password1.getValue();
            var pass2 = this.inp.Password2.getValue();
            var flag = true;
            var id =  this.inp.Id.getValue();
            if(id == null) {
                flag &= bsk.view.Form.AbstractForm.customFormCheckRequired(this.inp.Password1);
                flag &= bsk.view.Form.AbstractForm.customFormCheckRequired(this.inp.Password2);
                flag &= bsk.view.Form.AbstractForm.customFormPassCheck(this.inp.Password1, this.inp.Password2);
            }
            else {
                flag &= bsk.view.Form.AbstractForm.customFormPassCheck(this.inp.Password1, this.inp.Password2);
            }

            flag &= bsk.view.Form.AbstractForm.customFormChkLength(1, 50, this.inp.Login);
            flag &= bsk.view.Form.AbstractForm.customFormChkLength(1, 50, this.inp.Firstname);
            flag &= bsk.view.Form.AbstractForm.customFormChkLength(1, 50, this.inp.Lastname);
            flag &= bsk.view.Form.AbstractForm.customFormChkLength(1, 50, this.inp.Patronimic);

            /**
                TODO  Проверка введенного E-mail.
            **/

            return flag;
        },

        /**
            Формирует данные для сервера
        **/
        _uploadData : function(e) {
            this.base(arguments, e);
            var groupIdList = this.groupList.getSelectedId();
            var password = this.inp.Password1.getValue();
            if(this.validateForm()) {
                this.uReq.setParameter("password", password, true);
                this.uReq.setParameter("groups", groupIdList, true);
            }
        },

        /**
            Заполняет форму
        **/

        fillForm : function(data) {
            for(var fieldName in this.inp){
               if(("Password1" == fieldName) || ("Password2" == fieldName))
                    continue;
                var item = fieldName.toLowerCase();
                this.inp[fieldName].setValue(data.value[item])
                console.log("item = ", item);
            }

            this.inp.Password1.setValue("");
            this.inp.Password2.setValue("");
            this.groupList.setChecked(data.groups);
            
            var _id = data.value["id"];                    
            this.picForm.setParameter("id", _id);
        }
    }
});

