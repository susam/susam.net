properties([
    parameters([
        string(
            name: 'INPUT',
            defaultValue: '4, 3, 5, 4, 5, 8, 7, 9, 1',
            description: 'Comma-separated list of integers'
        )
    ])
])

def MAX_INPUT_SIZE = 10

node {
    echo "INPUT: ${params.INPUT}"
    currentBuild.description = "${params.INPUT} -> ..."

    def numbers = params.INPUT.split('\\s*,\\s*').collect {it as int}
    if (numbers.size() > MAX_INPUT_SIZE) {
        echo "ERROR: Input must not contain more than ${MAX_INPUT_SIZE} integers"
    }

    def pivot = numbers[0]
    def others = numbers.drop(1)
    def lo = others.findAll { it <= pivot }
    def hi = others.findAll { it > pivot }
    def builds = [:]
    def results = [lo: [], hi: []]

    if (lo) {
        builds.lo = {
            results.lo = build(
                job: env.JOB_NAME,
                parameters: [string(name: 'INPUT', value: lo.join(', '))
            ]).getBuildVariables().RESULT.split('\\s*,\\s*') as List
        }
    }
    if (hi) {
        builds.hi = {
            results.hi = build(
                job: env.JOB_NAME,
                parameters: [string(name: 'INPUT', value: hi.join(', '))
            ]).getBuildVariables().RESULT.split('\\s*,\\s*') as List
        }
    }
    parallel builds

    env.RESULT = (results.lo + [pivot] + results.hi).join(', ')
    echo "RESULT: ${env.RESULT}"
    currentBuild.description = "${params.INPUT} -> ${env.RESULT}"
}
