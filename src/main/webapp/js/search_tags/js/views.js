/**
 * Created with JetBrains WebStorm.
 * User: alex
 * Date: 18.01.13
 * Time: 23:53
 * To change this template use File | Settings | File Templates.
 */


App.searchModule.Views.Tag = Backbone.View.extend({
    tagName : 'li',
    className: 'tag tag-in-list',
    events : {
        'click' : 'choseTag'
    },
    choseTag : function(){
        this.model.set({chosen :true});
        App.searchModule.Vent.trigger('chosenTag',this.model);
        App.logger('chosenTag',this.model);
        //this.$el.hide();
    },
    handleChange : function(){
        this.$el.toggle();
    },
    initialize : function(){
        _.bindAll(this);
        this.model.on('change : chosen',this.handleChange);
    },
    render : function(){
        this.$el.append(App.searchModule.Config.tagTpl({tag:this.model.get('tag'),chosen : this.model.get('chosen')}));
        return this;
    }
});

App.searchModule.Views.ChosenTagView = Backbone.View.extend({
    tagName : 'li',
    className: 'tag chosen-tag',
    events : {
        'click .cls' : 'removeTag'
    },
    removeTag : function(){
        App.searchModule.Vent.trigger('removedTag',this.model);
        App.logger('chosenTag',this.model);

        this.$el.remove();
        //this.model.destroy();
        this.model.set({chosen: false});
    },
    initialize : function(){
        _.bindAll(this);
    },
    render : function(){
        this.$el.append( App.searchModule.Config.tagTpl({tag : this.model.get('tag'),chosen : this.model.get('chosen') } ));
        return this;
    }
});


App.searchModule.Views.SearchBox = Backbone.View.extend({

    //className : 'search-box',
    events : {
        'click #search-start': 'launchSearch'
    },
    launchSearch : function(){
        var query = $('#query-box').val();

        var tags = this.collection.pluck('tag');
        //callSomeFunction(tags, query)
        App.logger(tags ,"Search params");
        App.logger( query,"Search params");
    },
    initialize : function(){
        _.bindAll(this);
        this.setElement($('#search-box'));
        this.collection = new App.searchModule.Collections.Tag();
        App.searchModule.Vent.on('chosenTag',this.addTag);
        this.collection.on('add',this.renderTag);
        App.searchModule.Vent.on('removedTag',this.removeTag);
        this.render();
        this.list= this.$el.children('ul');

    },
    removeTag : function(tag){
        this.collection.remove(tag);
    },
    renderTag : function(tag){
        var chosenTagView = new App.searchModule.Views.ChosenTagView({model:tag});
        this.list.append(chosenTagView.render().$el);
    },
    addTag : function(tag){
        this.collection.add(tag);
    },
    render : function(){
        this.$el.html(App.searchModule.Config.serchBoxTpl);
        //this.list = this.$el.append('<ul></ul>').addClass('chosen-tag-list');
        return this;
    }
});
App.searchModule.Views.Category = Backbone.View.extend({
    tagName : 'li',
    className : 'category',
    events : {
        'click  .cat-title ,i': 'toggleTags'
    },
    toggleTags :function(){
        this.tagList.slideToggle();
        this.plus.toggleClass('icon-chevron-right').toggleClass(' icon-chevron-down');
    },
    initialize : function(){
        _.bindAll(this);
        //this.setElement($('#myModal'));
        this.collection =  new App.searchModule.Collections.Tag( this.model.get('tags'));
    },
    render : function(){
        this.plus = $('<i></i>').addClass(' icon-chevron-right icon-black').appendTo(this.$el);
        this.title = $('<div></div>').addClass('cat-title').appendTo(this.$el).html(this.model.get('name'));

        this.tagList = $('<ul></ul>').addClass('tag-list').appendTo(this.$el);
        this.collection.each(this.addTag);

        return this;
    },
    addTag : function(tag){
        var tagView = new App.searchModule.Views.Tag({model :tag});
        this.tagList.append(tagView.render().$el);
    }
});

App.searchModule.Views.tagSet = Backbone.View.extend({
    //tagName: 'ul',
    // className : 'cat-list',
    events : {
    },

    initialize : function(){
        _.bindAll(this);
        this.setElement($('#cat-holder'));
        this.render();
    },
    render : function(){
        this.$el.append('<h3>Cathegories</h3>');

        //this.list = this.$el.append('<ul></ul>').addClass('cat-list');
        this.list= $('<ul></ul>').appendTo(this.$el).addClass('cat-list');
        this.collection.each(this.addCategory);
        //$('#cat-holder').append(this.$el);
        App.logger(this.list ,"list");
    },
    addCategory:function(cat){
        var catView = new App.searchModule.Views.Category({model : cat});
        this.list.append(catView.render().$el);
    }
});