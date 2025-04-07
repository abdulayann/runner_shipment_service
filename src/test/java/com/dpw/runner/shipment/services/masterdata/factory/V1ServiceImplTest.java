package com.dpw.runner.shipment.services.masterdata.factory;

import com.dpw.runner.shipment.services.masterdata.helper.ICarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.helper.impl.mapper.CarrierMasterDataImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class V1ServiceImplTest {

    @Mock
    private CarrierMasterDataImpl mapperMasterData;

    @InjectMocks
    private CarrierMasterDataFactory masterDataFactory;

    @BeforeEach
    void setUp() {
        ReflectionTestUtils.setField(masterDataFactory, "source", "");
    }

    @Test
    void testGetCarrierMasterDataService() {
        ICarrierMasterData result = masterDataFactory.getCarrierMasterDataService();
        assertEquals(mapperMasterData, result);
    }

    @Test
    void testGetCarrierMasterDataService2() {
        ReflectionTestUtils.setField(masterDataFactory, "source", "Mapper");
        ICarrierMasterData result = masterDataFactory.getCarrierMasterDataService();
        assertEquals(mapperMasterData, result);
    }

    @Test
    void testGetCarrierMasterDataService3() {
        ReflectionTestUtils.setField(masterDataFactory, "source", "Mapper2");
        ICarrierMasterData result = masterDataFactory.getCarrierMasterDataService();
        assertEquals(mapperMasterData, result);
    }
}
