var toggleBtn = new Shiny.InputBinding();
$.extend(toggleBtn, {
  find: function(scope) {
    return $(scope).find(".sbs-toggle-button");
  },
  getValue: function(el) {
    return $(el).hasClass("active");
  },
  subscribe: function(el, callback) {
    var self = $(el);
    
    self.on("click", function(e) {
      self.toggleClass("active").blur();
      callback();
      if (self.hasClass('active')){
        self.html(self.data('canceltext'));
      }
    })
  },
  unsubscribe: function(el) {
    $(el).off("click");
  }
});
Shiny.inputBindings.register(toggleBtn);

Shiny.addCustomMessageHandler("fmButtonUpdate", function(data) {
  var btn = $("button#" + data.id);
  var ico = btn.find("i");
  
  if (ico.length > 0) {
    ico = ico[0].outerHTML;
  } else {
    ico = "";
  };
  
  if (data.hasOwnProperty("label")) {
    btn.html(ico + data.label);
  };
  
  if (data.hasOwnProperty("icon")) {
    var ch = btn.children();
    if (ch.length == 0) {
      btn.prepend(data.icon);
    } else {
      btn.find("i").replaceWith(data.icon);
    };
  };
  
  if (data.hasOwnProperty("value")) {
    if (btn.hasClass("sbs-toggle-button")) {
      if (data.value != btn.hasClass("active")) {
        btn.trigger("click");
      };
    };
  };
  
  if (data.hasOwnProperty("style")) {
    btn
      .removeClass("btn-default btn-primary btn-success btn-info btn-warning btn-danger btn-link")
      .addClass("btn-" + data.style);
  };
  
  if (data.hasOwnProperty("size")) {
    btn.removeClass("btn-lg btn-sm btn-xs")
    if (data.size != "default") {
      btn.addClass(data.size);
    };
  };
  
  if (data.hasOwnProperty("block")) {
    btn.toggleClass("btn-block", data.block);
  };
  
  if (data.hasOwnProperty("disabled")) {
    if (data.disabled) {
      btn.attr("disabled", "disabled");
    } else {
      btn.attr("disabled", false);
    };
  };
});
