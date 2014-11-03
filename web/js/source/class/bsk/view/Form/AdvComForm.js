/* ************************************************************************

************************************************************************ */


qx.Class.define("bsk.view.Form.AdvComForm",
{

    extend : bsk.view.Form.BaseForm,

    construct : function(controller, Row) {
        if(Row)
            this.createNew = (Row.isNew == true);
        this.base(arguments, controller, Row);
        this.addListeners();

        this.fStatus = "wdata";
        this.fData = null;
    },

    members : {
        inp : {
            Id              : null,
            Name            : null,
            Pic_url         : null,
            datestart       : null,
            datestop        : null,
            selBannerPlace  : null,
            ref             : null
        },

        urc : {  // upload request config
            url: "/update-adv-com",
            imgurl: "/update-adv-com/upload-image",
            method: "POST",
            mimetype: "application/json"
        },

        drc : {  // download request config
            url: "/get-adv-com",
            method: "GET",
            mimetype: "application/json"
        },

        buildForm : function(){
            this.inp.Id = null;
            this.inp.Name = new qx.ui.form.TextField();
            this.picButton = new bsk.view.Form.Upload.UploadButton("uploadfile", null, "icon/16/actions/document-save.png"),
            this.picForm = new bsk.view.Form.Upload.UploadForm('uploadFrm', this.urc.imgurl);

            this.inp.datestart = new qx.ui.form.DateField();
            this.inp.datestart.setValue(new Date());
            this.inp.datestop = new qx.ui.form.DateField()
            this.inp.datestop.setValue(new Date());

            this.inp.Pic_url = new qx.ui.form.TextField();

            this.inp.ref = new qx.ui.form.TextField();

            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;

            var layout = new qx.ui.layout.Grid(2, 1);
            var cnt = new qx.ui.container.Composite(layout);
            var l1 = new qx.ui.basic.Label("Общая информация");
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");
            this.inp.Name.focus();
            this.imgBanner = new qx.ui.basic.Image("");
/*            this.imgBanner.set({
                width : 960,
                height : 400
            });*/

            this.inp.selBannerPlace = new qx.ui.form.SelectBox();
 
            var vertical_offset = 0;

            l1.setFont("bold");
            l1.setAlignX("left");
            cnt.add(l1, {row:0, column:0, colSpan:2});
 
            cnt.add(new qx.ui.basic.Label().set({value: "Название:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.Name, {row:vertical_offset , column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "Дата начала:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.datestart, {row:vertical_offset , column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "Дата конца:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.datestop, {row:vertical_offset , column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "Расположение:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.selBannerPlace, {row:vertical_offset , column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "URL:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.ref, {row:vertical_offset , column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "Картинка:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this._buildPicFormCnt(), {row:vertical_offset , column:1});
            cnt.add(this.imgBanner, {row:++vertical_offset , column:0, colSpan:2});

            this.addbuttonRow(cnt, ++vertical_offset);

            this.controller.placeForm(cnt);

            var reqBannerPlaces = new qx.io.remote.Request("/get-banner-places", "GET", "application/json");
            reqBannerPlaces.addListener("completed", function(response) {
                var result = response.getContent();
                if (bsk.util.errors.process(this, result)==false) return false;
                this.inp.selBannerPlace.itemMap = [];
                for(var j=0; j<result.values.length; j++) {
                    var SI = result.values[j];
                    var selItem = new qx.ui.form.ListItem(SI.alias, null, SI.id);
                    this.inp.selBannerPlace.itemMap[SI.id] = selItem;
                    this.inp.selBannerPlace.add(selItem);
                }
                this.fStatus = 1;
                if(this.fData)
                    this.fillForm(this.fData);
            }, this);
            reqBannerPlaces.send();
            return {controller : cnt, offset: vertical_offset};
        },
                
        /**
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
                _this.imgBanner.setSource("/" + response);
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

        fillForm : function(data) {
            if(this.fStatus == 0) {
                this.fData = data;
                return;
            }

            var dtStart = bsk.util.utils.getDateLocal(data.datestart, 0);
            var dtStop = bsk.util.utils.getDateLocal(data.datestop, 0);

            this.inp.Id = data.id;
            this.inp.Name.setValue(data.name);
            this.inp.ref.setValue(data.ref);
            this.inp.Pic_url.setValue(data.url);
            this.inp.datestart.setValue(dtStart);
            this.inp.datestop.setValue(dtStop);
            var bannerId = data.banner_place_id;
            var item = this.inp.selBannerPlace.itemMap[bannerId];
            this.inp.selBannerPlace.setSelection([item]);
            if(data.url && data.url != "")
                this.imgBanner.setSource("/" + data.url);
        },
        
        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = true;

            flag &= bsk.view.Form.AbstractForm.customFormChkLength(1, bsk.Config.DOC_NAME_MAX_LEN, this.inp.Name);

            return flag;
        },
        
        _uploadData : function(e) {
//            this._dropInvalid();
            var res = {
                id : this.inp.Id,
                pic_url : this.inp.Pic_url.getValue(),
                datestart : this.inp.datestart.getValue().getTime(),
                datestop : this.inp.datestop.getValue().getTime(),
                name : this.inp.Name.getValue(),
                ref : this.inp.ref.getValue(),
                banner_place_id : this.inp.selBannerPlace.getSelection()[0].getModel()
            };

            if(this.validateForm()) {
                this.uReq = new qx.io.remote.Request(this.urc.url, this.urc.method, this.urc.mimetype);
                for(var item in res){
                    this.uReq.setParameter(item, res[item], true);
                }
            }
        }
    }
});

