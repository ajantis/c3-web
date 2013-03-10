/**
 * Created with JetBrains WebStorm.
 * User: alex
 * Date: 15.01.13
 * Time: 15:52
 * To change this template use File | Settings | File Templates.
 */

App.searchModule = {
    Collections:{},
    Models:{},
    Views:{},
    Config:{
        testdata : [
            {
                name : 'Arthropoda',
                id :1,
                tags : [
                    {
                        tag :'mandibula',
                        id :1
                    },

                    {
                        tag :'antenna',
                        id :2
                    },
                    {
                        tag :'crustacea',
                        id :4
                    },
                    {
                        tag :'onychphora',
                        id :23
                    }
                ]
            },
            {
                name : 'Synapsida',
                id :3,
                tags : [
                    {
                        tag :'Therapsida',
                        id :12
                    },

                    {
                        tag :'sequemata',
                        id :22
                    },
                    {
                        tag :'insectivora',
                        id :14
                    }

                ]
            },
            {
                name : 'Pisces',
                id :4,
                tags : [
                    {
                        tag :'hyomandibula',
                        id :54
                    },

                    {
                        tag :'tetrapodamorpha',
                        id :27
                    },
                    {
                        tag :'dipnoi',
                        id :42
                    },
                    {
                        tag :'tiiktaallik',
                        id :63
                    }
                ]
            }
        ],
        tagTpl :_.template("<div class='label label-info'><span><%=tag%></span>" +
            "<%if(chosen){%><a class='close cls'>×</a><%}%></div>"),
        serchBoxTpl : "<h4>You are searching by these tags :</h4>" +
            "<input type='text' placeholder= 'Enter query text' id='query-box' > " +
            "<a id='search-start' class='btn '>Search</a>  " +
            "<ul id = 'chosen-tag-list'></ul>"
    }
};

App.searchModule.init =function(){
    this.Vent = _.extend({},Backbone.Events);
    this.dataSet = new App.searchModule.Collections.Category(App.searchModule.Config.testdata);
    this.tagSet = new App.searchModule.Views.tagSet({collection :this.dataSet });

    this.searchBox = new App.searchModule.Views.SearchBox();
    //this.searchBox.render().$el.appendTo('body');
};

$('document').ready(function(){
    App.searchModule.init();
} );