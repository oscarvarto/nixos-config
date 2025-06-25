#!/usr/bin/env nu

# TestNG Debug Wrapper Script (Nushell version)
# This script sets up the proper classpath for TestNG debugging with DAP

# Take all arguments as rest parameters to avoid flag parsing issues
def main [...testng_args] {
    let args = $testng_args

    # Get the project root (find pom.xml)
    let project_root = if ("pom.xml" | path exists) {
        pwd
    } else if ("../pom.xml" | path exists) {
        cd ..
        pwd
    } else {
        # Look for pom.xml up the directory tree
        mut current_dir = (pwd)
        while $current_dir != "/" and not (($current_dir | path join "pom.xml") | path exists) {
            $current_dir = ($current_dir | path dirname)
        }
        if not (($current_dir | path join "pom.xml") | path exists) {
            error make {msg: "Could not find pom.xml in current directory or parent directories"}
        }
        $current_dir
    }

    print $"Using project root: ($project_root)"
    cd $project_root

    # Get Maven dependencies
    let maven_cp = (^mvn dependency:build-classpath -q | lines | last)

    # Define local repository path
    let repo = ($env.HOME | path join ".m2" "repository")

    # TestNG and its dependencies
    let testng_jar = ($repo | path join "org" "testng" "testng" "7.11.0" "testng-7.11.0.jar")
    let jcommander_jar = ($repo | path join "org" "jcommander" "jcommander" "1.83" "jcommander-1.83.jar")
    let jquery_jar = ($repo | path join "org" "webjars" "jquery" "3.7.1" "jquery-3.7.1.jar")
    let slf4j_jar = ($repo | path join "org" "slf4j" "slf4j-api" "2.0.17" "slf4j-api-2.0.17.jar")
    let logback_classic_jar = ($repo | path join "ch" "qos" "logback" "logback-classic" "1.5.18" "logback-classic-1.5.18.jar")
    let logback_core_jar = ($repo | path join "ch" "qos" "logback" "logback-core" "1.5.6" "logback-core-1.5.6.jar")

    # Project classes
    let project_classes = ($project_root | path join "target" "classes")
    let test_classes = ($project_root | path join "target" "test-classes")

    # Construct full classpath
    let full_cp = [
        $project_classes,
        $test_classes,
        $maven_cp,
        $testng_jar,
        $jcommander_jar,
        $jquery_jar,
        $slf4j_jar,
        $logback_classic_jar,
        $logback_core_jar
    ] | str join ":"

    # Ensure project is compiled
    if not ($test_classes | path exists) {
        print "Compiling project..."
        ^mvn test-compile -q
    }

    # Debug output
    print $"TestNG classpath length: (($full_cp | str length))"
    print $"Running: java -cp [classpath] org.testng.TestNG ($args | str join ' ')"

    # Run TestNG with the constructed classpath
    ^java -cp $full_cp org.testng.TestNG ...$args
}
