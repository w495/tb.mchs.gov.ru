
/* ************************************************************************

#asset(qx/icon/Tango/32/actions/go-previous.png)
#asset(qx/icon/Tango/32/actions/system-log-out.png)
#asset(qx/icon/Tango/32/actions/list-add.png)
#asset(qx/icon/Tango/32/actions/zoom-in.png)
#asset(qx/icon/Tango/32/apps/office-chart.png)
#asset(qx/icon/Tango/32/apps/preferences-users.png)
#asset(qx/icon/Tango/32/actions/edit-delete.png)


#asset(qx/icon/Tango/22/actions/go-previous.png)
#asset(qx/icon/Tango/22/actions/system-log-out.png)
#asset(qx/icon/Tango/22/actions/list-add.png)
#asset(qx/icon/Tango/22/actions/zoom-in.png)
#asset(qx/icon/Tango/22/apps/office-chart.png)
#asset(qx/icon/Tango/22/apps/preferences-users.png)
#asset(qx/icon/Tango/22/actions/edit-delete.png)

#asset(qx/icon/Tango/16/actions/go-previous.png)
#asset(qx/icon/Tango/16/actions/system-log-out.png)
#asset(qx/icon/Tango/16/actions/list-add.png)
#asset(qx/icon/Tango/16/actions/list-remove.png)
#asset(qx/icon/Tango/16/actions/zoom-in.png)
#asset(qx/icon/Tango/16/apps/office-chart.png)
#asset(qx/icon/Tango/16/apps/preferences-users.png)
#asset(qx/icon/Tango/16/actions/edit-delete.png)

************************************************************************ */

/**
 * The main tool bar widget
 */
qx.Class.define("bsk.view.ToolBar",
{
  //extend : qx.ui.toolbar.ToolBar,
    extend : qx.ui.container.Composite,

    construct : function(biz, cntl, model)
    {
        this.biz = biz;
        this.cntl = cntl;
        this.model = model;
        this.base(arguments, new qx.ui.layout.Dock());
        
        this.cnt = new qx.ui.container.Composite(new qx.ui.layout.HBox(12));
        this.add(this.cnt, {edge: "west"});

        if(model != undefined && (model[0] == undefined || model[0] != "woexit")) {
            this.cntRight = new qx.ui.container.Composite(new qx.ui.layout.HBox(12));
            this.add(this.cntRight, {edge: "east"});
            this.logOutBtn = new qx.ui.toolbar.Button("Выход", "icon/32/actions/system-log-out.png");
            this.cntRight.add(this.logOutBtn, {flex:0});
            this.logOutBtn.addListener("execute", this._onLogOutBtnClick, this);
        }

        this.buildToolbar(model);
    },

    members : {

        buildToolbar : function(model) {
            for(var k in model) {
                var E = model[k];
                if(E.type != undefined){
                    switch(E.type) {
                        case "button" :
                            var btn = new qx.ui.toolbar.Button(E.name, E.icon||"icon/32/actions/list-add.png");
                            btn.setAlignY("top");
                            this.cnt.add(btn, { flex : 0});
                            btn.e_action = E.action;
                            btn.e_specparam = E.specParam;
                            btn.e_descr = E;
                            var own = this;
                            btn.addListener("execute", function(e) {
                                var t = e.getTarget();
                                own._onBtnClick(t.e_action, t.e_specparam, t.e_descr);
                            })
                            break;
                        case "back" :
                            var sz = 32;
                            if(E.size != undefined)
                                sz = E.size;
                            this.goBackBtn = new qx.ui.toolbar.Button("Назад", "icon/" + sz + "/actions/go-previous.png");
                            this.goBackBtn.setAlignY("top");
                            this.cnt.add(this.goBackBtn, {flex:0});
                            this.goBackBtn.addListener("execute", this._onGoBackBtnClick, this);
                            if(this.biz.history.length > 0)
                                this.goBackBtn.setEnabled(true)
                            else
                                this.goBackBtn.setEnabled(false);

                            break;
                    }
                }
            }
        },

        _onBtnClick : function(action, specParam, descr) {
       //     action();
            this.cntl.onToolbarBtn(action, specParam, descr);
        },

        _onGoBackBtnClick : function(e) {
            this.biz.back();
        },
        _onLogOutBtnClick : function(e) {
            var logout_req = new qx.io.remote.Request("/do_logout", "GET", "application/json");
            logout_req.addListener("completed", function(result){ location.reload() }, this);
            logout_req.send();
        }
    }
});

