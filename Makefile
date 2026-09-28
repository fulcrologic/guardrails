tests:
	pnpm install --frozen-lockfile
	pnpm exec shadow-cljs -A:dev compile ci-tests
	pnpm exec karma start --single-run
	clojure -A:dev:test:clj-tests -J-Dguardrails.config=guardrails-test.edn -J-Dguardrails.enabled
