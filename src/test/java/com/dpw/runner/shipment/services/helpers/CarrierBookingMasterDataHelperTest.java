package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CarrierBookingMasterDataHelperTest {

    @Mock
    MasterDataUtils masterDataUtils;
    @Mock
    CommonUtils commonUtils;
    @Mock
    MasterDataKeyUtils masterDataKeyUtils;
    @InjectMocks
    CarrierBookingMasterDataHelper helper;

    // real executor used for tests (so runAsync actually executes)
    ExecutorService realExecutor;

    @BeforeEach
    void setup() {
        // make withMdc return the original runnable so runAsync executes it
        Mockito.lenient().when(masterDataUtils.withMdc(any(Runnable.class)))
                .thenAnswer(invocation -> invocation.getArgument(0));

        // create a real executor and inject into helper (field is package-private in production code)
        realExecutor = Executors.newSingleThreadExecutor();
        helper.executorServiceMasterData = realExecutor;
    }

    @AfterEach
    void teardown() {
        if (realExecutor != null && !realExecutor.isShutdown()) {
            realExecutor.shutdownNow();
        }
    }

    @Test
    void testGetMasterDataForList_whenGetMasterData_true_callsMasterDataUtilsMethods() {
        List<IRunnerResponse> responseList = new ArrayList<>();
        doNothing().when(masterDataUtils).setLocationData(anyList(), anyString());
        doNothing().when(masterDataUtils).fetchVesselForList(anyList());
        doNothing().when(masterDataUtils).fetchCarriersForList(anyList());
        doNothing().when(masterDataUtils).fetchTenantIdForList(anyList());

        helper.getMasterDataForList(Collections.emptyList(), responseList, true, true, Set.of("col1"));

        // verify underlying calls happened
        verify(masterDataUtils, atLeastOnce()).setLocationData(eq(responseList), anyString());
        verify(masterDataUtils, atLeastOnce()).fetchVesselForList(eq(responseList));
        verify(masterDataUtils, atLeastOnce()).fetchCarriersForList(eq(responseList));
        verify(masterDataUtils, atLeastOnce()).fetchTenantIdForList(eq(responseList));
    }

    @Test
    void testAddAllMasterDataInSingleCall_populatesMasterDataResponse() {
        CarrierBookingResponse resp = new CarrierBookingResponse();
        resp.setSailingInformation(new SailingInformationResponse());

        Map<String, Object> masterDataResponse = new HashMap<>();

        MasterListRequest dummyReq = new MasterListRequest();
        when(masterDataUtils.createInBulkMasterListRequest(
                any(), any(), anyMap(), anyString(), anyMap()))
                .thenReturn(List.of(dummyReq));

        when(masterDataUtils.fetchInBulkMasterList(any()))
                .thenReturn(Collections.emptyMap());

        doNothing().when(masterDataUtils).pushToCache(anyMap(), anyString(), anySet(), any(), anyMap());
        doNothing().when(masterDataKeyUtils).setMasterDataValue(anyMap(), anyString(), anyMap(), anyMap());

        helper.addAllMasterDataInSingleCall(resp, masterDataResponse);

        verify(masterDataUtils, times(1)).fetchInBulkMasterList(any());
        verify(masterDataUtils, times(1)).pushToCache(anyMap(), eq(CacheConstants.MASTER_LIST), anySet(), any(), anyMap());
        verify(masterDataKeyUtils, times(1)).setMasterDataValue(anyMap(), eq(CacheConstants.MASTER_LIST), eq(masterDataResponse), anyMap());
    }
}