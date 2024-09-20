package com.dpw.runner.shipment.services.masterdata.factory;

import com.dpw.runner.shipment.services.masterdata.helper.IMasterDataService;
import com.dpw.runner.shipment.services.masterdata.helper.impl.mapper.MapperMasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class MasterDataFactoryTest {

    @Mock
    private MapperMasterDataImpl mapperMasterData;

    @Mock
    private V1MasterDataImpl v1MasterData;

    @InjectMocks
    private MasterDataFactory masterDataFactory;

    @BeforeEach
    void setUp() {
        ReflectionTestUtils.setField(masterDataFactory, "source", "");
    }

    @Test
    void testGetMasterDataService_WhenSourceIsEmpty_ShouldReturnMapperMasterData() {
        ReflectionTestUtils.setField(masterDataFactory, "source", "");
        IMasterDataService result = masterDataFactory.getMasterDataService();
        assertEquals(mapperMasterData, result);
    }

    @Test
    void testGetMasterDataService_WhenSourceIsV1MasterData_ShouldReturnV1MasterData() {
        ReflectionTestUtils.setField(masterDataFactory, "source", "v1");
        IMasterDataService result = masterDataFactory.getMasterDataService();
        assertEquals(v1MasterData, result);
    }

    @Test
    void testGetMDMServiceBean_ShouldReturnMapperMasterData() {
        IMasterDataService result = masterDataFactory.getMDMServiceBean();
        assertEquals(mapperMasterData, result);
    }

    @ParameterizedTest
    @CsvSource({
            "'', MAPPER_MASTER_DATA",
            "Mapper, MAPPER_MASTER_DATA",
            "UNKNOWN, MAPPER_MASTER_DATA"
    })
    void testGetMasterDataService(String source, String expectedService) {
        ReflectionTestUtils.setField(masterDataFactory, "source", source);
        IMasterDataService expected = "MAPPER_MASTER_DATA".equals(expectedService) ? mapperMasterData : v1MasterData;
        IMasterDataService result = masterDataFactory.getMasterDataService();
        assertEquals(expected, result);
    }
}
