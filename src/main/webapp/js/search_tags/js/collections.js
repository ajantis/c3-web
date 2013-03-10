/**
 * Created with JetBrains WebStorm.
 * User: alex
 * Date: 19.01.13
 * Time: 2:39
 * To change this template use File | Settings | File Templates.
 */
App.searchModule.Collections.Tag = Backbone.Collection.extend({
    model : App.searchModule.Models.Tag
});

App.searchModule.Collections.Category = Backbone.Collection.extend({
    model :App.searchModule.Models.Category
});