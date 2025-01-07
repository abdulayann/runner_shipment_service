package com.dpw.runner.shipment.services.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.UpdateConsoleShipmentRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.AutoUpdateWtVolRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculateContainerSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculateShipmentSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerAssignListRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentConsoleIdDto;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentContainerAssignRequest;
import com.dpw.runner.shipment.services.dto.request.AttachListShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.CheckCreditLimitFromV1Request;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentOrderAttachDetachRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.billing.InvoicePostingValidationRequest;
import com.dpw.runner.shipment.services.dto.request.notification.PendingNotificationRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGApprovalRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequest;
import com.dpw.runner.shipment.services.dto.response.AllShipmentCountResponse;
import com.dpw.runner.shipment.services.dto.response.UpstreamDateUpdateResponse;
import com.dpw.runner.shipment.services.dto.v1.request.PartiesOrgAddressRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TIContainerListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TIListRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.DpsException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.syncing.AuditLogsSyncRequest;
import com.dpw.runner.shipment.services.syncing.Entity.CustomShipmentSyncRequest;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentReverseSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import javax.servlet.http.HttpServletResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentControllerTest {

    @Mock
    private IShipmentService shipmentService;
    @Mock
    private IShipmentSync shipmentSync;
    @Mock
    private IShipmentReverseSync shipmentReverseSync;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private ObjectMapper objectMapper;
    @Mock
    private IOrderManagementAdapter orderManagementAdapter;
    @Mock
    private IConsolidationService consolidationService;
    @Mock
    private IDpsEventService dpsEventService;
    @InjectMocks
    private ShipmentController shipmentController;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        mockMvc = MockMvcBuilders.standaloneSetup(shipmentController).build();
    }

    /**
     * Method under test: {@link ShipmentController#create(ShipmentRequest)}
     */

    @Test
    void createTest() {
        // Mock
        when(shipmentService.create(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(1));
        // Test
        var responseEntity = shipmentController.create(new ShipmentRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void hblCheck() {
        // Mock
        when(shipmentService.hblCheck(anyString(), anyString())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.hblCheck("DBA-12-111", Optional.of("sampleShipmentId"));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createTest2() {
        // Mock
        when(shipmentService.create(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(1));
        // Test
        var responseEntity = shipmentController.create(new ShipmentRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createTest3() {
        // Mock
        when(shipmentService.create(any())).thenThrow(new RuntimeException("RuntimeException"));
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(1));
        // Test
        var responseEntity = shipmentController.create(new ShipmentRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#update(ShipmentRequest)}
     */

    @Test
    void updateTest() throws RunnerException {
        // Mock
        when(shipmentService.update(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.update(new ShipmentRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateTest2() throws RunnerException {
        // Mock
        when(shipmentService.update(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.update(new ShipmentRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateTest3() throws RunnerException {
        // Mock
        when(shipmentService.update(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.update(new ShipmentRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#partialUpdate(Object, Boolean)}
     */

    @Test
    void partialUpdateTest() throws RunnerException {
        // Mock
        when(shipmentService.partialUpdate(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.partialUpdate(new ShipmentRequest(), false);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdateTest2() throws RunnerException {
        // Mock
        when(shipmentService.partialUpdate(any(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.partialUpdate(new ShipmentRequest(), false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdateTest3() throws RunnerException {
        // Mock
        when(shipmentService.partialUpdate(any(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.partialUpdate(new ShipmentRequest(), false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchOrgInfoTest_Success() throws RunnerException {
        PartiesOrgAddressRequest request = PartiesOrgAddressRequest.builder().build();

        PartiesRequest response = PartiesRequest.builder().build();
        when(shipmentService.fetchOrgInfoFromV1(request)).thenReturn(response);

        var responseEntity = shipmentController.fetchOrgInfo(request);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchOrgInfoTest_Exception() throws RunnerException {
        PartiesOrgAddressRequest request = PartiesOrgAddressRequest.builder().build();
        when(shipmentService.fetchOrgInfoFromV1(request)).thenThrow(new RunnerException("EX"));

        var responseEntity = shipmentController.fetchOrgInfo(request);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, responseEntity.getStatusCode());
    }

    @Test
    void fetchOrgInfoTest_Exception2() throws RunnerException {
        PartiesOrgAddressRequest request = PartiesOrgAddressRequest.builder().build();
        when(shipmentService.fetchOrgInfoFromV1(request)).thenThrow(new RunnerException());

        var responseEntity = shipmentController.fetchOrgInfo(request);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, responseEntity.getStatusCode());
    }
    /**
     * Method under test: {@link ShipmentController#toggleLock(Long)}
     */

    @Test
    void toggleLockTest() throws RunnerException {
        // Mock
        when(shipmentService.toggleLock(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.toggleLock(123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void toggleLockTest2() throws RunnerException {
        // Mock
        when(shipmentService.toggleLock(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.toggleLock(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void toggleLockTest3() throws RunnerException {
        // Mock
        when(shipmentService.toggleLock(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.toggleLock(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#createV1Shipment(ShipmentRequest)}
     */

    @Test
    void createV1ShipmentTest() throws RunnerException {
        // Mock
        when(shipmentService.completeV1ShipmentCreateAndUpdate(any(), any(), any(), anyBoolean(), any(), any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        // Test
        var responseEntity = shipmentController.createV1Shipment(new ShipmentRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createV1ShipmentTest2() throws RunnerException {
        // Mock
        when(shipmentService.completeV1ShipmentCreateAndUpdate(any(), anyMap(), anyList(), anyBoolean(), anyList(), anyString())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        // Test
        var responseEntity = shipmentController.createV1Shipment(new ShipmentRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createV1ShipmentTest3() throws RunnerException {
        // Mock
        when(shipmentService.completeV1ShipmentCreateAndUpdate(any(), anyMap(), anyList(), anyBoolean(), anyList(), anyString())).thenThrow(new RuntimeException("RuntimeException"));
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(new ShipmentRequest());
        // Test
        var responseEntity = shipmentController.createV1Shipment(new ShipmentRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#calculateContainerSummary(CalculateContainerSummaryRequest)}
     */

    @Test
    void calculateContainerSummaryTest() throws RunnerException {
        // Mock
        when(shipmentService.calculateContainerSummary(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.calculateContainerSummary(new CalculateContainerSummaryRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateContainerSummaryTest2() throws RunnerException {
        // Mock
        when(shipmentService.calculateContainerSummary(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.calculateContainerSummary(new CalculateContainerSummaryRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculateContainerSummaryTest3() throws RunnerException {
        // Mock
        when(shipmentService.calculateContainerSummary(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.calculateContainerSummary(new CalculateContainerSummaryRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#calculatePackSummary(CalculatePackSummaryRequest)}
     */

    @Test
    void calculatePackSummaryTest() throws RunnerException {
        // Mock
        when(shipmentService.calculatePackSummary(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.calculatePackSummary(new CalculatePackSummaryRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculatePackSummaryTest2() throws RunnerException {
        // Mock
        when(shipmentService.calculatePackSummary(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.calculatePackSummary(new CalculatePackSummaryRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculatePackSummaryTest3() throws RunnerException {
        // Mock
        when(shipmentService.calculatePackSummary(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.calculatePackSummary(new CalculatePackSummaryRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#calculateAutoUpdateWtVolInShipment(AutoUpdateWtVolRequest)}
     */

    @Test
    void calculateAutoUpdateWtVolInShipmentTest() throws RunnerException {
        // Mock
        when(shipmentService.calculateAutoUpdateWtVolInShipment(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.calculateAutoUpdateWtVolInShipment(new AutoUpdateWtVolRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateAutoUpdateWtVolInShipmentTest2() throws RunnerException {
        // Mock
        when(shipmentService.calculateAutoUpdateWtVolInShipment(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.calculateAutoUpdateWtVolInShipment(new AutoUpdateWtVolRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculateAutoUpdateWtVolInShipmentTest3() throws RunnerException {
        // Mock
        when(shipmentService.calculateAutoUpdateWtVolInShipment(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.calculateAutoUpdateWtVolInShipment(new AutoUpdateWtVolRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#calculateWtVolInShipmentOnChanges(AutoUpdateWtVolRequest)}
     */

    @Test
    void calculateWtVolInShipmentOnChangesTest() throws RunnerException {
        // Mock
        when(shipmentService.calculateWtVolInShipmentOnChanges(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.calculateWtVolInShipmentOnChanges(new AutoUpdateWtVolRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void calculateWtVolInShipmentOnChangesTest2() throws RunnerException {
        // Mock
        when(shipmentService.calculateWtVolInShipmentOnChanges(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.calculateWtVolInShipmentOnChanges(new AutoUpdateWtVolRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculateWtVolInShipmentOnChangesTest3() throws RunnerException {
        // Mock
        when(shipmentService.calculateWtVolInShipmentOnChanges(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.calculateWtVolInShipmentOnChanges(new AutoUpdateWtVolRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#syncShipmentToService(CustomShipmentSyncRequest, boolean, boolean)}
     */

    @Test
    void syncShipmentToServiceTest() {
        // Mock
        when(shipmentReverseSync.reverseSync(any(), anyBoolean(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.syncShipmentToService(new CustomShipmentSyncRequest(), false, false);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void syncShipmentToServiceTest2() {
        // Mock
        when(shipmentReverseSync.reverseSync(any(), anyBoolean(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.syncShipmentToService(new CustomShipmentSyncRequest(), false, false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncShipmentToServiceTest3() {
        // Mock
        when(shipmentReverseSync.reverseSync(any(), anyBoolean(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.syncShipmentToService(new CustomShipmentSyncRequest(), false, false);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#getCustomShipment(ShipmentDetails)}
     */

    @Test
    void getCustomShipmentTest() throws RunnerException {
        // Mock
        when(shipmentSync.sync(any(), any(), any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.getCustomShipment(new ShipmentDetails());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getCustomShipmentTest2() throws RunnerException {
        // Mock
        when(shipmentSync.sync(any(), anyList(), anyList(), anyString(), anyBoolean())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.getCustomShipment(new ShipmentDetails());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getCustomShipmentTest3() throws RunnerException {
        // Mock
        when(shipmentSync.sync(any(), anyList(), anyList(), anyString(), anyBoolean())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.getCustomShipment(new ShipmentDetails());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#getAllMasterData(Long)}
     */

    @Test
    void getAllMasterDataTest() {
        // Mock
        when(shipmentService.getAllMasterData(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.getAllMasterData(123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterDataTest2() {
        // Mock
        when(shipmentService.getAllMasterData(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.getAllMasterData(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterDataTest3() {
        // Mock
        when(shipmentService.getAllMasterData(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.getAllMasterData(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testFetchByQuery() {
        // Mock
        when(shipmentService.fetchShipments(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));
        // Test
        var responseEntity = shipmentController.fetchByQuery(ListCommonRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createTestShipment() throws RunnerException {
        // Mock
        when(shipmentService.createTestShipment(any())).thenReturn(List.of());
        // Test
        var responseEntity = shipmentController.createTestRecord(1);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void delete() throws RunnerException {
        // Mock
        when(shipmentService.delete(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.delete(1L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testList() throws RunnerException {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));
        when(shipmentService.fullShipmentsList(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.list(ListCommonRequest.builder().build(), true, true);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testList1() throws RunnerException {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));
        when(shipmentService.list(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.list(ListCommonRequest.builder().build(), false, true);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testList3() throws RunnerException {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));
        when(shipmentService.list(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.list(ListCommonRequest.builder().build(), false, true);
        // Assert
        assertEquals(HttpStatus.FORBIDDEN, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById() {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));
        when(shipmentService.retrieveById(any(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.retrieveById(Optional.of(111L), Optional.of(UUID.randomUUID().toString()), true);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveForNTE() {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));
        when(shipmentService.retrieveForNTE(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.retrieveForNTE(Optional.of(111L));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCompleteRetrieveById() throws ExecutionException, InterruptedException {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));
        when(shipmentService.completeRetrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.completeRetrieveById(Optional.of(111L), List.of(), Optional.of(UUID.randomUUID().toString()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#completeUpdate(ShipmentRequest)}
     */

    @Test
    void completeUpdateTest() throws RunnerException {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));
        when(shipmentService.completeUpdate(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.completeUpdate(new ShipmentRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void completeUpdateTest2() throws RunnerException {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));
        when(shipmentService.completeUpdate(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.completeUpdate(new ShipmentRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void completeUpdateTest3() throws RunnerException {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));
        when(shipmentService.completeUpdate(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.completeUpdate(new ShipmentRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void assignShipmentContainers()  {
        // Mock
        when(shipmentService.assignShipmentContainers(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.assignShipmentContainers(new ShipmentContainerAssignRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void assignAllContainers()  {
        // Mock
        when(shipmentService.assignAllContainers(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.assignAllContainers(new ContainerAssignListRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#syncShipmentAuditLogsToService(AuditLogsSyncRequest)}
     */

    @Test
    void syncShipmentAuditLogsToServiceTest() {
        // Mock
        when(shipmentService.syncShipmentAuditLogsToService(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.syncShipmentAuditLogsToService(new AuditLogsSyncRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void syncShipmentAuditLogsToServiceTest2() {
        // Mock
        when(shipmentService.syncShipmentAuditLogsToService(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.syncShipmentAuditLogsToService(new AuditLogsSyncRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncShipmentAuditLogsToServiceTest3() {
        // Mock
        when(shipmentService.syncShipmentAuditLogsToService(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.syncShipmentAuditLogsToService(new AuditLogsSyncRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void cloneById()  {
        // Mock
        when(shipmentService.cloneShipment(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.cloneById(123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#listTransportInstruction(TIListRequest)}
     */

    @Test
    void transportInstructionListTest() {
        // Mock
        when(shipmentService.transportInstructionList(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.listTransportInstruction(new TIListRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void transportInstructionListTest2() {
        // Mock
        when(shipmentService.transportInstructionList(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.listTransportInstruction(new TIListRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void transportInstructionListTest3() {
        // Mock
        when(shipmentService.transportInstructionList(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.listTransportInstruction(new TIListRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#listContainersForTI(TIContainerListRequest)}
     */

    @Test
    void listContainersForTITest() {
        // Mock
        when(shipmentService.containerListForTI(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.listContainersForTI(new TIContainerListRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listContainersForTITest2() {
        // Mock
        when(shipmentService.containerListForTI(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.listContainersForTI(new TIContainerListRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listContainersForTITest3() {
        // Mock
        when(shipmentService.containerListForTI(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.listContainersForTI(new TIContainerListRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link ShipmentController#exportShipmentList(HttpServletResponse, ListCommonRequest)}
     */

    @Test
    void exportShipmentListTest() throws IOException, IllegalAccessException, ExecutionException, InterruptedException {
        boolean isExecuted = true;
        // Mock
        doNothing().when(shipmentService).exportExcel(any(), any());
        // Test
        shipmentController.exportShipmentList(new MockHttpServletResponse(), new AttachListShipmentRequest());
        // Assert
        assertTrue(isExecuted);
    }

    @Test
    void exportShipmentListTest2() throws IOException, IllegalAccessException, ExecutionException, InterruptedException {
        boolean isExecuted = true;
        // Mock
        doThrow(new RuntimeException()).when(shipmentService).exportExcel(any(), any());
        // Test
        shipmentController.exportShipmentList(new MockHttpServletResponse(), new AttachListShipmentRequest());
        // Assert
        assertTrue(isExecuted);
    }

    @Test
    void exportShipmentListTest3() throws IOException, IllegalAccessException, ExecutionException, InterruptedException {
        boolean isExecuted = true;
        // Mock
        doThrow(new RuntimeException("RuntimeException")).when(shipmentService).exportExcel(any(), any());
        // Test
        shipmentController.exportShipmentList(new MockHttpServletResponse(), new AttachListShipmentRequest());
        // Assert
        assertTrue(isExecuted);
    }

    @Test
    void retrieveByOrderId() throws RunnerException {
        // Mock
        when(shipmentService.retrieveByOrderId(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.retrieveByOrderId("123L");
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getDefaultShipment() throws RunnerException {
        // Mock
        when(shipmentService.getDefaultShipment()).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.getDefaultShipment();
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void generateCustomHouseBLNumber() throws RunnerException {
        // Mock
        when(shipmentService.generateCustomHouseBLNumber()).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.generateCustomHouseBLNumber();
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getConsolFromShipment() throws RunnerException {
        // Mock
        when(consolidationService.getConsolFromShipment(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.getConsolFromShipment(123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getConsolFromShipment2() throws RunnerException {
        // Mock
        when(consolidationService.getConsolFromShipment(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.getConsolFromShipment(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void attachListShipment() throws RunnerException {
        // Mock
        when(shipmentService.attachListShipment(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.attachListShipment(new AttachListShipmentRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void attachListShipment2() throws RunnerException {
        // Mock
        when(shipmentService.attachListShipment(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.attachListShipment(new AttachListShipmentRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getMasterDataDescriptionMapping() throws RunnerException {
        // Mock
        when(shipmentService.getMasterDataMappings()).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.getMasterDataDescriptionMapping();
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getIdFromGuid() throws RunnerException {
        // Mock
        when(shipmentService.getIdFromGuid(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.getIdFromGuid(UUID.randomUUID().toString());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getIdFromGuid2() throws RunnerException {
        // Mock
        when(shipmentService.getIdFromGuid(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.getIdFromGuid(UUID.randomUUID().toString());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getGuidFromId() throws RunnerException {
        // Mock
        when(shipmentService.getGuidFromId(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.getGuidFromId(111L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getGuidFromId2() throws RunnerException {
        // Mock
        when(shipmentService.getGuidFromId(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.getGuidFromId(111L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void checkCreditLimitFromV1() throws RunnerException {
        // Mock
        when(shipmentService.checkCreditLimitFromV1(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.checkCreditLimitFromV1(new CheckCreditLimitFromV1Request());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void checkCreditLimitFromV12() throws RunnerException {
        // Mock
        when(shipmentService.checkCreditLimitFromV1(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.checkCreditLimitFromV1(new CheckCreditLimitFromV1Request());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchShipmentsForConsoleId() throws RunnerException {
        // Mock
        when(shipmentService.fetchShipmentsForConsoleId(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.fetchShipmentsForConsoleId(11L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchActiveInvoices() throws RunnerException {
        // Mock
        when(shipmentService.fetchActiveInvoices(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.fetchActiveInvoices(UUID.randomUUID().toString());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void showAssignAllContainers()  {
        // Mock
        when(shipmentService.showAssignAllContainers(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.showAssignAllContainers(new ShipmentConsoleIdDto());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchCreditLimit() throws RunnerException {
        // Mock
        when(shipmentService.fetchCreditLimit(any(), anyString())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.fetchCreditLimit("11L", "Default");
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchEmails() throws RunnerException {
        // Mock
        when(shipmentService.fetchEmails(any(), anyLong())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.fetchEmails(123L, 2121L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void validateInvoicePosting_success() {
        InvoicePostingValidationRequest request = new InvoicePostingValidationRequest();
        when(shipmentService.validateInvoicePosting(request)).thenReturn(ResponseHelper.buildSuccessResponse());

        var responseEntity = shipmentController.validateInvoicePosting(request);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

    }

    @Test
    void getDateTimeChanges() throws RunnerException {
        // Mock
        when(shipmentService.getDateTimeChangeUpdates(anyLong())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.getDateTimeChanges(123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getDateTimeChangesFails() throws RunnerException {
        // Mock
        when(shipmentService.getDateTimeChangeUpdates(anyLong())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.getDateTimeChanges(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getContainerListFromTrackingServiceFails() throws RunnerException {
        // Mock
        when(shipmentService.getContainerListFromTrackingService(anyLong(), anyLong())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.getContainerListFromTrackingService(123L,123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getContainerListFromTrackingService() throws RunnerException {
        // Mock
        when(shipmentService.getContainerListFromTrackingService(anyLong(),anyLong())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.getContainerListFromTrackingService(123L,123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testConsoleShipmentList_Success() {
        // Mock
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        Long consoleId = 1L;
        boolean isAttached = true;
        IRunnerResponse runnerResponse = new RunnerListResponse<>();
        ResponseEntity<IRunnerResponse> responseEntity = ResponseEntity.ok(runnerResponse);

        when(shipmentService.consoleShipmentList(any(CommonRequestModel.class), eq(consoleId), eq(""), eq(isAttached), anyBoolean()))
                .thenReturn(responseEntity);
        // Test
        responseEntity = shipmentController.consoleShipmentList(listCommonRequest, 1L, "", true, true);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testConsoleShipmentList_Exception() {
        // Mock
        when(shipmentService.consoleShipmentList(any(), anyLong(), eq(null),  anyBoolean(), anyBoolean())).thenThrow(new RuntimeException("Test Exception"));
        ListCommonRequest request = mock(ListCommonRequest.class);
        // Test
        var responseEntity = shipmentController.consoleShipmentList(request, 1L, null,true, true);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetAllShipments_Success() {
        Long consoleId = 1L;
        IRunnerResponse runnerResponse = new AllShipmentCountResponse();
        ResponseEntity<IRunnerResponse> responseEntity = ResponseEntity.ok(runnerResponse);

        when(shipmentService.getAllShipments(consoleId)).thenReturn(responseEntity);

        responseEntity = shipmentController.getAllShipments(consoleId);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetAllShipments_Exception() {
        Long consoleId = 1L;

        when(shipmentService.getAllShipments(consoleId)).thenThrow(new RuntimeException("Test Exception"));

        var responseEntity = shipmentController.getAllShipments(consoleId);

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateConsoleShipments_Success() throws RunnerException {
        IRunnerResponse runnerResponse = new UpstreamDateUpdateResponse();
        UpdateConsoleShipmentRequest request = mock(UpdateConsoleShipmentRequest.class);
        ResponseEntity<IRunnerResponse> responseEntity = ResponseEntity.ok(runnerResponse);

        when(shipmentService.updateShipments(any())).thenReturn(responseEntity);

        responseEntity = shipmentController.updateShipments(request);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateConsoleShipments_Exception() throws RunnerException {
        UpdateConsoleShipmentRequest request = mock(UpdateConsoleShipmentRequest.class);

        when(shipmentService.updateShipments(any())).thenThrow(new RuntimeException("Test Exception"));

        var responseEntity = shipmentController.updateShipments(request);

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @ParameterizedTest
    @ValueSource(strings = {Constants.SHIPMENT, Constants.CONSOLIDATION})
    void testRetrieveMeasurementBasis(String module) {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));

        if(module.equalsIgnoreCase(Constants.SHIPMENT)) {
            when(shipmentService.shipmentRetrieveWithMeasurmentBasis(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        } else {
            when(consolidationService.consolidationRetrieveWithMeasurmentBasis(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        }
        // Test
        var responseEntity = shipmentController.retrieveMeasurmentData(Optional.of(UUID.randomUUID().toString()), Optional.of(module));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRequestInterBranchConsole() throws RunnerException {
        // Mock
        when(shipmentService.requestInterBranchConsole(1L, 2L, "")).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.requestInterBranchConsole(1L, 2L, "");
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRequestInterBranchConsole_Failure() throws RunnerException {
        // Mock
        when(shipmentService.requestInterBranchConsole(1L, 2L, "")).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.requestInterBranchConsole(1L, 2L, "");
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getPendingNotifications() throws RunnerException {
        // Mock
        when(shipmentService.getPendingNotifications(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.getPendingNotifications(new PendingNotificationRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getPendingNotificationsFails() throws RunnerException {
        // Mock
        when(shipmentService.getPendingNotifications(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = shipmentController.getPendingNotifications(new PendingNotificationRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetAllConsolShipmentsLatestDate_Success() throws Exception {
        // Arrange
        Long consoleId = 1L;
        ResponseEntity<IRunnerResponse> expectedResponse = ResponseEntity.ok(new UpstreamDateUpdateResponse());
        when(shipmentService.getLatestCargoDeliveryDate(consoleId)).thenReturn(expectedResponse);

        // Test
        var responseEntity = shipmentController.getAllConsolShipmentsLatestDate(consoleId);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetAllConsolShipmentsLatestDate_Exception() throws Exception {
        // Arrange
        Long consoleId = 1L;
        String errorMessage = "Service error";
        when(shipmentService.getLatestCargoDeliveryDate(consoleId)).thenThrow(new RuntimeException(errorMessage));

        // Test
        var responseEntity = shipmentController.getAllConsolShipmentsLatestDate(consoleId);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateShipmentSummary() throws RunnerException {
        CalculateShipmentSummaryRequest calculateShipmentSummaryRequest = CalculateShipmentSummaryRequest.builder().build();
        // Mock
        when(shipmentService.calculateShipmentSummary(CommonRequestModel.buildRequest(calculateShipmentSummaryRequest))).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.calculateShipmentSummary(calculateShipmentSummaryRequest);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateShipmentSummary_Failure() throws RunnerException {
        CalculateShipmentSummaryRequest calculateShipmentSummaryRequest = CalculateShipmentSummaryRequest.builder().build();
        // Mock
        when(shipmentService.calculateShipmentSummary(CommonRequestModel.buildRequest(calculateShipmentSummaryRequest))).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.calculateShipmentSummary(calculateShipmentSummaryRequest);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testOceanDGSendForApproval() throws RunnerException {
        OceanDGApprovalRequest request = OceanDGApprovalRequest.builder().build();
        // Mock
        when(shipmentService.sendOceanDGApprovalEmail(request)).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.oceanDGSendForApproval(request);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testOceanDGApprovalResponse() throws RunnerException {
        OceanDGRequest request = OceanDGRequest.builder().build();
        // Mock
        when(shipmentService.dgApprovalResponse(request)).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.oceanDGApprovalResponse(request);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithoutTenantFilter() throws RunnerException {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().build();
        when(shipmentService.listWithoutTenantCheck(any())).thenThrow(new RuntimeException());
        var responseEntity = shipmentController.listWithoutTenantFilter(listCommonRequest);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCreateShipmentForBooking() throws RunnerException {
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().build();
        when(shipmentService.createShipmentFromBooking(any())).thenReturn("Success");
        var responseEntity = shipmentController.createShipmentForBooking(shipmentRequest);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreateShipmentForBookingException() throws RunnerException {
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().build();
        when(shipmentController.createShipmentForBooking(any())).thenThrow(new RuntimeException());
        assertThrows(RunnerException.class, () -> shipmentController.createShipmentForBooking(shipmentRequest));
    }

    @Test
    void testAttachDetachOrderSuccess() {
        ShipmentOrderAttachDetachRequest shipmentOrderAttachDetachRequest = ShipmentOrderAttachDetachRequest.builder().build();
        when(shipmentService.attachDetachOrder(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = shipmentController.attachDetachOrder(shipmentOrderAttachDetachRequest);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testAttachDetachOrderException(){
        ShipmentOrderAttachDetachRequest shipmentOrderAttachDetachRequest = ShipmentOrderAttachDetachRequest.builder().build();
        when(shipmentController.attachDetachOrder(any())).thenThrow(new RuntimeException());
        var responseEntity = shipmentController.attachDetachOrder(shipmentOrderAttachDetachRequest);
    }

    @Test
    void testCancelShipment() throws RunnerException {
        Long shipmentId = 1L;
        when(shipmentService.cancel(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = shipmentController.cancelShipment(shipmentId);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCancelShipmentThrowsException() throws RunnerException {
        Long shipmentId = 1L;
        String errorMessage = "Error";
        when(shipmentService.cancel(any())).thenThrow(new RuntimeException(errorMessage));
        var responseEntity = shipmentController.cancelShipment(shipmentId);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetFilteredShipment(){
        when(shipmentService.fetchBillChargesShipmentList(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = shipmentController.listBillChargesShipments("guid", "SH",null, null );
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetFilteredShipmentException() {
        when(shipmentService.fetchBillChargesShipmentList(any())).thenThrow(new RuntimeException());
        var responseEntity = shipmentController.listBillChargesShipments("guid", any(), null, null );
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getMatchingRulesByGuid() {
        String guid = UUID.randomUUID().toString();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(guid).build());
        // Mock
        when(dpsEventService.getShipmentMatchingRulesByGuid(guid)).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.getMatchingRulesByGuid(guid);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getMatchingRulesByGuid_Failure() {
        String guid = UUID.randomUUID().toString();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(guid).build());
        // Mock
        when(dpsEventService.getShipmentMatchingRulesByGuid((guid))).thenThrow(new DpsException());
        // Test
        assertThrows(DpsException.class, () -> {
            shipmentController.getMatchingRulesByGuid(guid);
        });
    }
    @Test
    void testListExternal() {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));
        when(shipmentService.fullShipmentsExternalList(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = shipmentController.listExternal(ListCommonRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testListExternal1() {
        // Mock
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));
        when(shipmentService.fullShipmentsExternalList(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shipmentController.listExternal(ListCommonRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.FORBIDDEN, responseEntity.getStatusCode());
    }

}
