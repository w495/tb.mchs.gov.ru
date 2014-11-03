qx.Class.define("bsk.view.BizUploadForm",
{
    extend : bsk.view.AbstractForm,

    construct : function(controller, Row) {
        this.base(arguments, controller);
        this.formRow = Row;

        this.MAXFAIL = 5;
        this.CHUNKSIZE = 5000000;

        this.uploads = {};
        this.buildForm();
    },

    members : {
        showEMsg : function(fieldName, msg) {
        },

        buildForm : function() {
            var tabView = new qx.ui.tabview.TabView();
            var page1 = new qx.ui.tabview.Page("Проекты");
            page1.setLayout(new qx.ui.layout.VBox());
            tabView.add(page1);

            var page2 = new qx.ui.tabview.Page("Статус задач");
            page2.setLayout(new qx.ui.layout.VBox());
            tabView.add(page2);

            var page3 = new qx.ui.tabview.Page("Загрузка");
            page3.setLayout(new qx.ui.layout.VBox());
            tabView.add(page3);


            var layout = new qx.ui.layout.VBox(12);
            var cnt = new qx.ui.container.Composite(layout);

            cnt.add(tabView, {flex:1});


            var model = {
                tabHeight : 80,
                columns : [
                    {name : "cn", alias : "название", type : "string", sortable : true},
                    {name : "avprojectalias", alias : "alias", type : "string", sortable : true},
                    {name : "bitrate", alias : "битрейт", type : "string", sortable : true},
                    {name : "datetime", alias : "дата изменения", type : "string", sortable : true},
                    {name : "country", alias : "страны", type : "string", sortable : true},
                    {name : "ro", alias : "правообладатель", type : "string", sortable : true},
                    {name : "expire", alias : "expire date", type : "string", sortable : true}
                ],
                filter : {
                    fields : [
                        {name : "host",   alias : "сервер", input : {
                            type : "select-box",
                            select_id : "host",
                            select_display_name : "host",
                            values_url : "/get-bizs"
                        }}
                    ],
                    withCancel : false,
                    render : "horisontal",
                    submit_btn_label : "получить список",
                    submit_url : "/get-biz-film"
                },
                sort : "avurn",
                ascending : false,
                selectionMode : 4,
                index_name : ["host"]
            };

            this.leftTab = new bsk.view.TabController(this, undefined, undefined, model);
//            this.leftTab.setWidth(450);
//            this.leftTab.setHeight(400);
            page1.add(this.leftTab, {flex:1});

            var model2 = {
                tabHeight : 80,
                columns : [
                    {name : "filename", alias : "название", type : "string", sortable : true},
                    {name : "percent", alias : "процент", type : "percent", sortable : true, tofixed:2},
                    {name : "datetime", alias : "начало загрузки", type : "erl_datetime_utc", sortable : true},
                    {name : "status", alias : "статус", type : "string", sortable : true}
                ],
                filter : {
                    fields : [
                        {name : "host",   alias : "сервер", input : {
                            type : "select-box",
                            select_id : "host",
                            select_display_name : "host",
                            values_url : "/get-bizs"
                        }}
                    ],
                    withCancel : false,
                    render : "horisontal",
                    submit_btn_label : "получить список",
                    //submit_url : "/get-biz-film"
                    submit_url : "/get-biz-uploads"
                },
                sort : "filename",
                ascending : false,
                index_name : ["host"]
            };

            this.uploadsTab = new bsk.view.TabController(this, undefined, undefined, model2);
//            this.uploadsTab.setWidth(350);
//            this.uploadsTab.setHeight(400);
            page2.add(this.uploadsTab, {flex:1});

            
            var buttonRow = new qx.ui.container.Composite();
            buttonRow.setMarginTop(5);
            var hbox = new qx.ui.layout.HBox(5);
            hbox.setAlignX("right");
            buttonRow.setLayout(hbox);

            var btnList = new qx.ui.form.Button("Получить список выбранных проектов");
//            var btnUpload = new qx.ui.form.Button("Загрузить");
            var btnDelete = new qx.ui.form.Button("Удалить");
            var btnSync = new qx.ui.form.Button("Синхронизировать серверы");

            buttonRow.add(btnSync);
//            buttonRow.add(btnUpload);
            buttonRow.add(btnDelete);
            buttonRow.add(btnList);

/*            btnUpload.addListener("execute", function(e) {
                var z = this.leftTab.filterForm.getValues();
                new bsk.view.UploadDialog(this.controller.biz, z.host);
            }, this);
*/
            btnDelete.addListener("execute", this.onDelete, this);
            btnList.addListener("execute", this.onList, this);
            btnSync.addListener("execute", this.onSync, this);

            page1.add(buttonRow);

            var tableModel = this._tableModel = new qx.ui.table.model.Simple();
            tableModel.setColumns(
                ["id", "название", "куда", "начало загрузки", "битрейт", "правообладатель", "страна", "статус", "информация"],
                ["id", "filename", "target", "datetime", "bitrate", "owner", "country", "status", "descr"]
            );
            this.upTab = new qx.ui.table.Table(tableModel);

            this.upTab.setStatusBarVisible(false);
            this.upTab.setColumnVisibilityButtonVisible(false);

            this.upTab.getSelectionModel().setSelectionMode(qx.ui.table.selection.Model.MULTIPLE_INTERVAL_SELECTION);

            page3.add(this.upTab, {flex:1});

            var buttonRow3 = new qx.ui.container.Composite();
            buttonRow3.setMarginTop(5);
            var hbox3 = new qx.ui.layout.HBox(5);
            hbox3.setAlignX("right");
            buttonRow3.setLayout(hbox3);

            var btnUpload = new qx.ui.form.Button("Загрузить");
            var btnRetry = new qx.ui.form.Button("Повторить попытку");
            var btnStop = new qx.ui.form.Button("Удалить задачу");
            var btnPause = new qx.ui.form.Button("Приостановить загрузку");

            buttonRow3.add(btnUpload);
            buttonRow3.add(btnRetry);
            buttonRow3.add(btnStop);
            buttonRow3.add(btnPause);

            var own = this;
            btnUpload.addListener("execute", function(e) {
                var d = new bsk.view.UploadDialog(this.controller.biz);
                d.addListener("startUpload", own._onStartUpload, own);
            }, this);

            btnRetry.addListener("execute", this._retryUpload, this);
            btnStop.addListener("execute", this._stopUpload, this);
            btnPause.addListener("execute", this._pauseUpload, this);

            page3.add(buttonRow3);

            this.controller.placeForm(cnt);
        },

        iterSelection : function(fun) {
            var sm = this.upTab.getSelectionModel();
            var ranges = sm.getSelectedRanges();
            var arr = this._tableModel.getDataAsMapArray();

            for(var i=0; i<ranges.length; i++) {
                var R = ranges[i];
                for(var j=R.minIndex; j<=R.maxIndex; j++) {
                    //this.uploads[arr[j].id].state = "paused"
                    fun(this.uploads[arr[j].id]);
                }
            }
        },

        _pauseUpload : function(e) {
            var own = this;
            this.iterSelection(function(st) {
                st.state = "paused";
                own.updateUploadTable(st);
            });
        },

        _retryUpload : function(e) {
            var own = this;
            this.iterSelection(function(st) {
                st.state = "retry";
                st.failure=0;
                own.uploadChunk(st);
                own.updateUploadTable(st);
            });
        },

        _stopUpload : function(e) {
            var own = this;
            this.iterSelection(function(st) {
                st.state = "deleted";
                own.updateUploadTable(st);
            });
        },

        _onStartUpload : function(e) {
            var data = e.getData();

            var arr = [];
            for(var i=0; i<data.files.length; i++) {
                var F = data.files[i];
                var id = Math.round(Math.random() * 100000000);
                var dt = new Date();
                var st = {
                    curpos : 0,
                    id : id,
                    file : F,
                    datetime : dt,
                    owner : data.owner,
                    bitrate : data.bitrate,
                    country : data.country,
                    biz : data.biz,
                    state : "uploading",
                    failure : 0
                };

                this.uploads[id] = st;

                var E = [
                    id, 
                    F.fileName, 
                    data.biz,
                    bsk.util.utils.formatJsDateTime(dt), 
                    data.bitrate,
                    data.owner,
                    data.country,
                    "",
                    "",
                    ""
                ];

                this.uploadChunk(st);

                arr.push(E);
            }

            this._tableModel.addRows(arr);
        },

        uploadChunk : function(st) {
            if( st.file.size - st.curpos < this.CHUNKSIZE)
                st.toLoad = st.file.size - st.curpos;
            else
                st.toLoad = this.CHUNKSIZE;

            var blob;
            
            try {
                blob = st.file.slice(st.curpos,  st.toLoad);
            }
            catch(e) {
                blob = st.file.mozSlice(st.curpos,  st.toLoad);
            }

            var own = this;
            var reader = new FileReader();
            reader.onload = function(evt){
                if (evt.target.readyState == FileReader.DONE) {
                    var req = new qx.io.remote.Request("/upload-chunk", "POST", "text/plain");

                    req.setParameter("biz", st.biz);
                    req.setParameter("country", st.country);
                    req.setParameter("owner", st.owner);
                    req.setParameter("bitrate", st.bitrate);

                    req.setData(evt.target.result);
                    var x = evt.target.result.length;
                    req.st = this.mySt;
                    req.addListener("completed", own._onChunkUploaded, own);
                    req.addListener("timeout", own._onChunkFailed, own);
                    req.addListener("failed", own._onChunkFailed, own);
                    req.setTimeout(1000 * 60);

                    req.setRequestHeader("content-range", st.curpos + "-" + (st.curpos + st.toLoad) + "/" + st.file.size);
                    req.setRequestHeader("Content-Disposition", "attachment; filename=\"" + st.file.fileName + "\"");
                    req.setRequestHeader("Session-ID", st.id);
                    req.send();

                }
            };//.bind(this);

            reader.mySt = st;
            //reader.readAsBinaryString(blob);
            reader.readAsDataURL(blob);
        },

        _onChunkUploaded : function(e) {
            var st = e._target.st;
            if(st.state == "retry") {
                st.failure = 0;
                st.state = "uploading";
            }
            if(e.getStatusCode() == 201) {
                var cc = e.getContent();
                var arr1 = e.getContent().split("\n");
                var arr2 = [];
                for(var i=0; i<arr1.length; i++) {
//                    var E = arr[i];
                    var E = arr1[i].split("/")[0].split("-");
                    arr2.push([E[0]*1, E[1]*1]);
                }

                var arr3 = arr2.sort(function(a, b) {
                    return a[0] - b[0];
                });

                var min = arr3[0][1];
                for(var i=1; i<arr3.length; i++) {
                    var E = arr3[i];
                    if(min < E[0])
                        break;
                    else if(min < E[1])
                        min = E[1];
                }
                st.curpos = min;
                if(st.state == "uploading")
                    this.uploadChunk(st);
            }
            else if(e.getStatusCode() == 200) {
                st.state = "done";
            }

            this.updateUploadTable(st);
        },

        _onChunkFailed : function(e) {
            var st = e._target.st;
            if(st.state != "retry" || st.state != "uploading")
                return;
            if(st.failure + 1 < this.MAXFAIL) {
                st.failure += 1;
                this.uploadChunk(st);
                st.state = "retry";
            }
            else {
                st.state = "error";
            }
            this.updateUploadTable(st);
        },

        updateUploadTable : function(st) {
            var arr = this._tableModel.getDataAsMapArray();
            var d = null;
            var i;
            for(i=0; i<arr.length; i++) {
                if(arr[i].id == st.id) {
                    d = arr[i];
                    break;
                }
            }

            if(d) {
                switch(st.state) {
                    case "uploading":
                        d.status = "загружается";
                        d.descr = Math.round((st.curpos / st.file.size)*100) + "%";
                        //d.control = ;
                        break;
                    case "retry":
                        d.status = "повтор " + (this.MAXFAIL+1-st.failure);
                        d.descr = Math.round((st.curpos / st.file.size)*100) + "%";
                        //d.control = ;
                        break;
                    case "error":
                        d.status = "ошибка загрузки";
                        d.descr = "";
                        //d.control = ;
                        break;
                    case "done":
                        d.status = "завершено";
                        d.descr = "";
                        //d.control = ;
                        break;
                    case "paused":
                        d.status = "остановлено";
                        d.descr = Math.round((st.curpos / st.file.size)*100) + "%";
                        //d.control = ;
                        break;
                }
                if(st.state != "deleted") {
                    this._tableModel.setDataAsMapArray(arr);
                }
                else {
                    this._tableModel.removeRows(i, 1);
                }
            }
        },

        onSync : function() {
            var z = this.leftTab.filterForm.getValues();
            var req = new qx.io.remote.Request("/sync-all", "POST", "application/json");
            req.setParameter("biz", z.host);
            req.send();
            alert("Выполняется синхронизация.");
        },

        onList : function() {
            var sel = this.leftTab.tab.getAllSelected();
            var html = "<table cellpadding='4' style='border:1px solid black;font-size: x-small;'>";
            html += "<thead>";
            html += "<tr style='font-size: x-small;'>";
            html += "<th>Название</th>";
            html += "<th>alias</th>";
            html += "</tr>";
            html += "<tr>";
            html += "<td colspan='2'><hr style='color:black;border:1px solid black;height:1px;'/></td>";
            html += "</tr>";
            html += "</thead>";
            html += "<tbody>";
        
            for(var i=0; i<sel.length; i++) {
                html += "<tr>";
                html += "<td>" + sel[i].cn + "</td>";
                html += "<td>" + sel[i].avprojectalias + "</td>";
                html += "</tr>";
            }
            html += "</tbody>";
            html += "</table>";
            var w = window.open();
            var doc = w.document;
            doc.open();
            doc.write(html);
            doc.close();
        },

        onDelete : function() {
            var sel = this.leftTab.tab.getAllSelected();
            var z = this.leftTab.filterForm.getValues();

            for(var i=0; i<sel.length; i++) {
                if(confirm("Вы уверены что хотите удалить " + sel[i].cn + "?")) {
                    var req = new qx.io.remote.Request("/delete", "POST", "application/json");
                    req.setParameter("biz", z.host);
                    req.setParameter("cn", sel[i].cn);
                    req.send();
                }
            }
            this.refresh();
        },

        loadActionModel : function(ActionUrl) {
            alert("error1");
        },

        onAction : function(Row, FilterVal, ActionUrl) {
            alert("3");
        },

        back : function() {
            alert(4);
        },

        hide_global_pb : function() {
            document.getElementById("global_progress_bar").style.visibility="hidden";
        },

        show_global_pb : function() {
            document.getElementById("global_progress_bar").style.visibility="visible";
        },

        refresh : function() {
            this.leftTab.refresh();
            this.rightTab.refresh();
        },

        _dropInvalid : function() {
            for(var i = 0; i<this.fFields.length; i++) {
                this.fFields[i].setValid(true);
            }
        },

        _onCancelClick : function(e) {
            this.controller.onCancelClick();
        },

        loadFormData : function(url, id, paramName) {
            var req = new qx.io.remote.Request(url, "GET", "application/json");
            req.setParameter(paramName, id);
            req.addListener("completed", this._onLoadFormDataCompl, this);
            req.send();
        },

        _onLoadFormDataCompl : function(response) {
            var result = response.getContent();
            if (bsk.util.errors.process(this, result)==false) return false;
            this.fillForm(result);
        }
    }
});

