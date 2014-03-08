// ==UserScript==
// @name          Stack Exchange post monitor
// @description   a hacky bugfix of Stack Exchange questions not updating properly. 
// @description   currently fixes the posts not updating their deleted/undeleted status.
// @include       http://*stackexchange.com/*
// @include       http://*stackoverflow.com/*
// @include       http://*mathoverflow.com/*
// @include       http://*serverfault.com/*
// @include       http://*superuser.com/*
// @include       http://*stackapps.com/*
// @include       http://*askubuntu.com/*
// @version       2.0
// ==/UserScript==

(function () {
    
    if(!$.fn.addBack) $.fn.addBack = function(selector){
        return this.andSelf().filter(selector);    
    };

/// core

    function UpdateTask(target) {
        this.target = $(target);
        this.answerId = this.target.data("answerid");
        this.questionId = this.target.data("questionid");
        if (this.questionId) return;
        if (/^\/questions\/\d+/.test(location.pathname)){
            this.questionId =  this.target.closest("#close-question-form")
                                          .find(".question").data("questionid")
                            || $("#question").data("questionid");
        } else if (/^\/review\/low-quality-posts\/\d+/.test(location.pathname)){
            this.questionId = $(".question").data("questionid");
        }
    }

/// on new task

    UpdateTask.prototype.addSelf = function () {
        var thisTask = this,
            questionIdPromise = $.Deferred();
        updateTasks.push(this);
        updateTasksById[this.answerId || this.questionId] = this;
        if (this.questionId) {
            questionIdPromise.resolve(this.questionId);
        } else {
            //todo: this is ugly as a sin, even for a fallback. Use [the API][1] instead
            //[1]: http://api.stackexchange.com/docs/answers-by-ids
            $.get("/posts/" + this.answerId + "/edit").then(function (response) {
                questionIdPromise.resolve(
                    $(response).find("a.cancel-edit").attr("href").match(/questions\/(\d+)/)[1]
                );
            });
        }
        questionIdPromise.then(function (questionId) {
            if (!updateTasksByQuestionId[questionId]) updateTasksByQuestionId[questionId] = [];
            updateTasksByQuestionId[questionId].push(thisTask);
        });

        //deletion timer:
        if(this.target.is(".question-page #question")){
            $(document).on("ajaxComplete", function(_,_,ajaxOptions){
                if(/\/flags\//.test(ajaxOptions.url) && ajaxOptions.type === "POST"){
                    if(timerAttached){
                        console.log("attaching twice (???)");
                        return;
                    }
                    timerAttached = true;
                    thisTask.target.find(".post-menu").append($("<span>", {"class": "lsep", text: "|"}))
                        .append($("<input>", {type: "checkbox", id: "monitor-post-deletion", change: function(){
                            timeDeletion = updateAlways = this.checked;
                        }})).append($("<span>", {text: " time this post deletion"}));
                    thisTask.creationTime = Math.min.apply(Math,
                        thisTask.target.find(".user-action-time span[title]").map(function(){
                            return Date.parse(this.title);
                        })
                    );
                }
            });
        }
    };

/// on clock tick

    UpdateTask.updateAll = function () {
        var visibilityState = document.visibilityState || document.webkitVisibilityState || "visible";
        if (visibilityState === "visible" || updateAlways) {
            for (var questionId in updateTasksByQuestionId) {
                var updateTasksForQuestion = updateTasksByQuestionId[questionId];
                if (updateTasksForQuestion.some(function(x){return x.target.is(":visible")})) {
                    $.get("/questions/" + questionId, {
                        dataType: "html"
                    }).then(function (questionId, updateTasksForQuestion, response) {
                        $response = $(response);
                        if($(".pager-answers a", $response).length){
                            console.log("paging not supported");
                            return;
                        }
                        updateTasksForQuestion.forEach(function (updateTask) {
                            var $targetInResponse;
                            if (updateTask.answerId) {
                                $targetInResponse = $("#answer-" + updateTask.answerId, $response);
                            } else {
                                $targetInResponse = $("#question", $response);
                            }
                            //yep, even deleted questions are `.deleted-answer`s.
                            if ($targetInResponse.is(":not(.deleted-answer)")) { //and exists
                                updateTask.target.removeClass("deleted-answer");
                                
                            } else {
                                updateTask.target.addClass("deleted-answer");
                                // deletion times:
                                if(timeDeletion && $targetInResponse.is("#question")){
                                    updateTask.deletionTime = Date.parse(
                                        $(".question-status .relativetime", $response).attr("title")
                                    );
                                    updateTask.deletionTimeError = 500;
                                    timeDeletion = false;
                                    timerReport(updateTask);
                                }
                            }
                        });
                    }.bind(null, questionId, updateTasksForQuestion))
                    .fail(function (updateTasksForQuestion, jqxhr) {
                        if(jqxhr.status === 404){
                            updateTasksForQuestion.forEach(function(updateTask){
                                updateTask.target.addClass("deleted-answer");
                                if(timeDeletion && updateTask.target.is(".question-page #question")){
                                    updateTask.deletionTime = Date.now() - UPDATE_TIMEOUT / 2;
                                    updateTask.deletionTimeError = UPDATE_TIMEOUT/2;
                                    timeDeletion = false;
                                    timerReport(updateTask);
                                }
                            })
                        }else if (jqxhr.status > 0){
                            console.log(arguments);
                        }
                    }.bind(null,updateTasksForQuestion));
                }
            }
        }
    };

    /// globals

    var UPDATE_TARGET_SELECTOR = "div[data-answerid], div[data-questionid]",
        UPDATE_TIMEOUT = 10000,

        updateTasks = [],
        updateTasksById = {},
        updateTasksByQuestionId = {},

        updateAlways = false,
        timeDeletion = false,
        timerAttached = false;

    /// adding tasks

    $(UPDATE_TARGET_SELECTOR).each(function () {
        new UpdateTask(this).addSelf();
    });

    new MutationObserver(function (records) {
        records.forEach(function (record) {
            $(record.addedNodes)
                .find(UPDATE_TARGET_SELECTOR).addBack(UPDATE_TARGET_SELECTOR)
                .each(function () {
                new UpdateTask(this).addSelf();
            });
        });
    }).observe(document.body, {
        childList: true,
        subtree: true
    });

    setInterval(UpdateTask.updateAll, UPDATE_TIMEOUT);

    /// timer functions

    function timerReport(updateTask){
        debugger;
        $checkbox = $("#monitor-post-deletion");
        var dt = updateTask.deletionTime - updateTask.creationTime;
        var dte = updateTask.deletionTimeError;
        $checkbox.next().text(
            "deleted after " + (dt/1000).toFixed() + " +/- " + (dte/1000) + "s"
        );
        $checkbox.remove();
        document.title = "deleted - " + document.title;
    }
})();
