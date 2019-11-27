set(ConfigPackageLocation lib/cmake/PLN)

include(CMakePackageConfigHelpers)

configure_package_config_file(PLNConfig.cmake.in
	${CMAKE_CURRENT_BINARY_DIR}/PLNConfig.cmake
	INSTALL_DESTINATION lib/PLN/cmake
	PATH_VARS CMAKE_INSTALL_PREFIX
)

write_basic_package_version_file(
	"${CMAKE_CURRENT_BINARY_DIR}/PLNConfigVersion.cmake"
	VERSION ${SEMANTIC_VERSION}
	COMPATIBILITY SameMajorVersion
)

INSTALL(FILES
	${CMAKE_CURRENT_BINARY_DIR}/PLNConfigVersion.cmake
	${CMAKE_CURRENT_BINARY_DIR}/PLNConfig.cmake
	DESTINATION ${ConfigPackageLocation}
)
