package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Executors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.BILLING_DATA;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONTAINERS_LIST;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentMasterDataHelperV3Test {

    @InjectMocks
    ShipmentMasterDataHelperV3 shipmentMasterDataHelperV3;

    @Mock
    private MasterDataUtils masterDataUtils;

    @AfterEach
    void tearDown() {
        shipmentMasterDataHelperV3.executorServiceMasterData.shutdown();
    }
    @BeforeEach
    void setup() {
        shipmentMasterDataHelperV3.executorServiceMasterData = Executors.newFixedThreadPool(2);
    }


    @Test
    void testGetMasterDataForList() {
        List<ShipmentDetails> lst = new ArrayList<>(List.of(new ShipmentDetails()));
        List<IRunnerResponse> responseList = new ArrayList<>(List.of(new ShipmentListResponse()));
        Set<String> includeColumns = new HashSet<>();
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        shipmentMasterDataHelperV3.getMasterDataForList(lst, responseList, true, true, includeColumns);
        verify(masterDataUtils, times(3)).withMdc(any());
    }

    private void mockRunnable() {
        Runnable runnable = () -> System.out.println("Mock runnable executed");
        runnable.run();
    }

    @Test
    void testGetMasterDataForList1() {
        List<ShipmentDetails> lst = new ArrayList<>(List.of(new ShipmentDetails()));
        List<IRunnerResponse> responseList = new ArrayList<>(List.of(new ShipmentListResponse()));
        Set<String> includeColumns = new HashSet<>(List.of(CONTAINERS_LIST, BILLING_DATA));
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        shipmentMasterDataHelperV3.getMasterDataForList(lst, responseList, true, true, includeColumns);
        verify(masterDataUtils, times(5)).withMdc(any());
    }

    @Test
    void testGetMasterDataForList3() {
        List<ShipmentDetails> lst = new ArrayList<>(List.of(new ShipmentDetails()));
        List<IRunnerResponse> responseList = new ArrayList<>(List.of(new ShipmentListResponse()));
        Set<String> includeColumns = new HashSet<>(List.of(CONTAINERS_LIST, BILLING_DATA));
        shipmentMasterDataHelperV3.getMasterDataForList(lst, responseList, false, false, includeColumns);
        verify(masterDataUtils, times(0)).withMdc(any());
    }

}