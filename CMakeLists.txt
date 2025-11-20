cmake_minimum_required(VERSION 3.16)
project(AODSOLVE VERSION 1.0.0 LANGUAGES C CXX)
# project(AODSOLVE VERSION 1.0.0 LANGUAGES CXX)

# ------------------------------------------------------------------------------
# 基础配置
# ------------------------------------------------------------------------------
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
    add_compile_options(-Wall -Wextra -Wpedantic -O2 -g -fno-rtti)
endif()

# ------------------------------------------------------------------------------
# LLVM / Clang
# ------------------------------------------------------------------------------
find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "LLVMConfig.cmake at: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
include_directories(${CLANG_INCLUDE_DIRS})

llvm_map_components_to_libnames(LLVM_LIBRARIES
    core
    support
    option
)

set(CLANG_LIBRARIES
    clangTooling
    clangFrontend
    clangFrontendTool
    clangSerialization
    clangDriver
    clangParse
    clangSema
    clangAnalysis
    clangAST
    clangASTMatchers
    clangBasic
    clangLex
)

# ------------------------------------------------------------------------------
# 包含目录
# ------------------------------------------------------------------------------
include_directories(${CMAKE_SOURCE_DIR}/include)

# ------------------------------------------------------------------------------
# AOD 核心库
# ------------------------------------------------------------------------------
add_library(aod_core STATIC
    src/aod/core/enhanced_aod_node.cpp
    src/aod/core/enhanced_aod_graph.cpp
)

# ------------------------------------------------------------------------------
# AST 分析模块
# ------------------------------------------------------------------------------
add_library(aod_analysis STATIC
    src/analysis/enhanced_ast_analyzer.cpp
    src/analysis/integrated_cpg_analyzer.cpp
    src/analysis/function_inline_analyzer.cpp
    src/analysis/loop_vectorization_analyzer.cpp
)
target_link_libraries(aod_analysis aod_core)

# ------------------------------------------------------------------------------
# CPG 框架
# ------------------------------------------------------------------------------
add_library(aod_cpg STATIC
    src/analysis/CPGAnnotation.cpp
)
target_link_libraries(aod_cpg aod_core)

# ------------------------------------------------------------------------------
# 转换器
# ------------------------------------------------------------------------------
add_library(aod_converter STATIC
    src/conversion/enhanced_cpg_to_aod_converter.cpp
    src/conversion/enhanced_cpg_operator_conversion.cpp
    src/conversion/universal_optimization_engine.cpp
    src/conversion/optimization_rule_system.cpp
)
target_link_libraries(aod_converter aod_core aod_analysis aod_cpg)

# ------------------------------------------------------------------------------
# 代码生成器
# ------------------------------------------------------------------------------
add_library(aod_generator STATIC
    src/generation/enhanced_code_generator.cpp
)
target_link_libraries(aod_generator
    aod_core
    aod_converter
)

# ------------------------------------------------------------------------------
# 公共逻辑库（包含主分析器）
# ------------------------------------------------------------------------------
add_library(aodsolve_common STATIC
    src/tools/aodsolve_main_analyzer.cpp
)

target_link_libraries(aodsolve_common
    aod_generator
    aod_converter
    aod_analysis
    aod_cpg
    aod_core
    ${LLVM_LIBRARIES}
    ${CLANG_LIBRARIES}
)

# ------------------------------------------------------------------------------
# Demo 可执行程序
# ------------------------------------------------------------------------------
add_executable(vectorization_demo
    src/demo/enhanced_vectorization_demo.cpp
)

target_link_libraries(vectorization_demo aodsolve_common)

set_target_properties(vectorization_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# ------------------------------------------------------------------------------
# 安装规则
# ------------------------------------------------------------------------------
install(TARGETS vectorization_demo
    RUNTIME DESTINATION bin
)

install(TARGETS
    aod_core aod_analysis aod_cpg aod_converter aod_generator aodsolve_common
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)

# ------------------------------------------------------------------------------
# 构建前清理任务
# ------------------------------------------------------------------------------
add_custom_target(pre-build-clean
    COMMAND ${CMAKE_COMMAND} -E echo "Cleaning coverage data..."
    COMMAND find ${CMAKE_BINARY_DIR} -name "*.gcda" -type f -delete
    COMMENT "Removing old coverage data..."
)
add_dependencies(vectorization_demo pre-build-clean)

# ------------------------------------------------------------------------------
# 输出构建配置
# ------------------------------------------------------------------------------
message(STATUS "AODSOLVE Build Configuration:")
message(STATUS "  LLVM Version: ${LLVM_PACKAGE_VERSION}")
message(STATUS "  C++ Standard: ${CMAKE_CXX_STANDARD}")
