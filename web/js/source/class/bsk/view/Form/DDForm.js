/* ************************************************************************

    Класс описания формы для создания и редактирования
        директорий и файлов.

************************************************************************ */


qx.Class.define("bsk.view.Form.DDForm",
{
    type : "abstract",

    extend : bsk.view.Form.BaseForm,

    construct : function(controller, Row) {
        if(Row)
            this.createNew = (Row.isNew == true);

        if(this.createNew)
            this.drc = this.drc_create;
        this.base(arguments, controller, Row);
    },

    members : {

        /* Редактируем или создаем новое */
        createNew : false,

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

        /* Список аттачей  для документов */
        attachList: null,
        attachListOptios: {
            url:            "/get-attach-list",
            labelFieldName: "name",
            descrFieldName: "alt"
        },
        
        attachFormCnt : null,
        
        /* Виджеты для сопровождающей картикни */
        picForm: null,
        picButton: null,
        
        /**
            Проверка новизны сущности
        **/
        testCreate: function(Row) {
            this.createNew = (Row.isNew == true);
            return this.createNew;
        },

        buildForm : function(){
            
            if(this.inp.Id)
                this.inp.Id.setEnabled(false);

            if(this.inp.Content){
                this.inp.Content.setWidth(bsk.Config.DOC_FORM_WIDTH );
                this.inp.Content.setHeight(bsk.Config.DOC_FORM_HEIGHT);
            }
            
            /* Сопровождающая картинка */
            this.picButton = new bsk.view.Form.Upload.UploadButton("uploadfile", null, "icon/16/actions/document-save.png"),
            this.picForm = new bsk.view.Form.Upload.UploadForm('uploadFrm', this.urc.imgurl);
        
            this.attachList = new bsk.view.Form.Upload.AttachList(this,
                this.attachListOptios.url,
                this.attachListOptios.labelFieldName,
                this.attachListOptios.descrFieldName
            );
            
            this.attachFormCnt = new bsk.view.Form.Upload.AttachContainer(this.attachList);
            this.attachList.setHeight(50);
            
            var layout = new qx.ui.layout.Grid(2, 1);
            var cnt = new qx.ui.container.Composite(layout);

            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");
            
            var l1 = new qx.ui.basic.Label("Общая информация");
            l1.setFont("bold");
            l1.setAlignX("left");
            cnt.add(l1, {row:0, column:0, colSpan:2});
            
            this.controller.placeForm(cnt);
            this.inp.Name.focus();
            
            return cnt;
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
            Создает область загрузки аттачей.
        **/
        _buildAttachFormCnt: function() {
            return this.attachFormCnt;
        },
        
        
        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = true;

            flag &= bsk.view.Form.AbstractForm.customFormChkLength(1, bsk.Config.DOC_NAME_MAX_LEN, this.inp.Name);
            flag &= bsk.view.Form.AbstractForm.customFormChkLength(1, bsk.Config.DOC_CONT_MAX_LEN, this.inp.Content);

            return flag;
        },
        
        /**
            Применив некоторые преобразования загружает данные на сервер
        **/
        _uploadData : function(e) {
            this._dropInvalid();
            var res = {}
            for(var fieldName in this.inp){
                item = fieldName.toLowerCase()
                res[item] = this.inp[fieldName].getValue();
                
                if ("name" == item
                    || "content" == item
                    || "start" == item
                    || "stop" == item
                    || "date" == item
                    ) res[item] = bsk.util.utils.normalize(res[item])
                    
                if("published" == item ){
                    res[item] = this.inp[fieldName].getValue() ? "true" : "false";
                    continue;
                }
            }
            if(this.validateForm()) {
                this.uReq = new qx.io.remote.Request(this.urc.url, this.urc.method, this.urc.mimetype);
                for(var item in res){
                    this.uReq.setParameter(item, res[item], true);
                }
            }
        }
    }
});

