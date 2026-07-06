(ns kotoba.issue.run-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.issue.store :as store]
            [kotoba.issue.gate :as gate]
            [kotoba.issue.run :as run]))

(def default-autonomy {:read-only :auto :external-send :approve
                        :financial :approve :destructive :approve})

(defn- archive-plan-fn [run _ts]
  {:proposal {:id (str "prop-" (:run/id run)) :issue (:run/issue run)
              :kind :gmail/archive :risk :read-only}
   :note "archive it"})

(defn- draft-plan-fn [run _ts]
  {:proposal {:id (str "prop-" (:run/id run)) :issue (:run/issue run)
              :kind :gmail/draft :risk :external-send :rationale "draft a reply"}
   :note "draft a reply"})

(deftest new-run-starts-at-plan
  (is (= :plan (:phase (run/new-run {:run-id "r1" :issue-id "issue-1" :ts 0})))))

(deftest plan-emits-propose-effect-and-moves-to-act
  (let [r (run/new-run {:run-id "r1" :issue-id "issue-1" :ts 0})
        {:keys [run effects]} (run/step archive-plan-fn r {:type :advance :ts 1})]
    (is (= :act (:phase run)))
    (is (= [{:effect :issue/propose :run-id "r1"
             :proposal {:id "prop-r1" :issue "issue-1" :kind :gmail/archive :risk :read-only}}]
           effects))))

(deftest low-risk-proposal-auto-merges-to-done
  (let [s (store/mem-store)
        r0 (run/new-run {:run-id "r2" :issue-id "issue-1" :ts 0})
        {:keys [run effects]} (run/step archive-plan-fn r0 {:type :advance :ts 1})
        propose-effect (first effects)
        result (run/interpret-propose! s {:gmail/archive (fn [_] {:archived true})}
                                        default-autonomy propose-effect)
        {:keys [run]} (run/observe run result 2)]
    (is (= :done (:phase run)))
    (is (= :merged (:kotoba.issue.proposal/status (store/get-entity s :proposal "prop-r2"))))))

(deftest risky-proposal-waits-for-approval-then-resumes
  (let [s (store/mem-store)
        r0 (run/new-run {:run-id "r3" :issue-id "issue-1" :ts 0})
        {:keys [run effects]} (run/step draft-plan-fn r0 {:type :advance :ts 1})
        propose-effect (first effects)
        handlers {:gmail/draft (fn [_] {:drafted true})}
        result (run/interpret-propose! s handlers default-autonomy propose-effect)
        {:keys [run]} (run/observe run result 2)]
    (is (= :awaiting-approval (:phase run)))
    (is (= :proposed (:kotoba.issue.proposal/status (store/get-entity s :proposal "prop-r3")))
        "still unreviewed in the gate -- :awaiting-review is run/observe's routing decision, not a stored status"))
  ;; approve! in the gate directly, then resume the run
  (let [s (store/mem-store)
        r0 (run/new-run {:run-id "r4" :issue-id "issue-1" :ts 0})
        {:keys [run effects]} (run/step draft-plan-fn r0 {:type :advance :ts 1})
        propose-effect (first effects)
        handlers {:gmail/draft (fn [_] {:drafted true})}
        result (run/interpret-propose! s handlers default-autonomy propose-effect)
        {:keys [run]} (run/observe run result 2)
        _ (gate/approve! s (:proposal run) {:decider "jun"})
        merged (gate/merge! s handlers)
        {:keys [run]} (run/resume-after-review run :approve 3 {:merged merged})]
    (is (= :done (:phase run)))))

(deftest reject-resume-ends-in-error
  (let [r0 (run/new-run {:run-id "r5" :issue-id "issue-1" :ts 0})
        r (assoc r0 :phase :awaiting-approval :proposal "prop-r5")
        {:keys [run]} (run/resume-after-review r :reject 1 {})]
    (is (= :error (:phase run)))))

(deftest request-changes-resume-goes-back-to-plan
  (let [r0 (run/new-run {:run-id "r6" :issue-id "issue-1" :ts 0})
        r (assoc r0 :phase :awaiting-approval :proposal "prop-r6")
        {:keys [run]} (run/resume-after-review r :request-changes 1 {})]
    (is (= :plan (:phase run)))))

(deftest cancel-any-run
  (let [r0 (run/new-run {:run-id "r7" :issue-id "issue-1" :ts 0})
        {:keys [run]} (run/cancel r0 1 "operator stop")]
    (is (= :cancelled (:phase run)))
    (is (run/done? run))))
