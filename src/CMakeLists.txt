add_library(kdtreefortran
	KdTreeFortran.f90
	kdnode/KdNodeDistance.f90
	kdnode/KdNodeGetters.f90
	kdnode/KdNodeUtils.f90
	kdtree/KdTreeBuild.f90
	kdtree/KdTreeGetters.f90
	kdtree/KdTreeUtils.f90
	kdtree/search_modules/KdTreeRnn.f90
	kdtree/search_modules/KdTreeLinScan.f90
	kdtree/search_modules/KdTreeDBSCAN.f90
	kdtree/KdTreeModders.f90
)

target_include_directories(kdtreefortran PUBLIC
	$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
	$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(kdtreefortran PRIVATE OpenMP::OpenMP_Fortran)