package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShippingInstructionTest extends CommonMocks {
    @InjectMocks
    private ShippingInstructionService shippingInstructionService;
    @Mock
    private IShippingInstructionDao shippingInstructionDao;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private IPackingDao packingDao;
    @Mock
    private IReferenceNumbersDao referenceNumbersDao;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IContainerDao containerDao;
    @Mock
    private AuditLogService auditLogService;
    @Mock
    private IV1Service v1Service;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private TenantSettingsService tenantSettingsService;
    @Mock
    private IPartiesDao partiesDao;
    @Mock
    private IEventDao eventDao;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static ShippingInstructionRequest shippingInstructionRequest;
    private static ShippingInstruction shippingInstruction;


    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build()
        );
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
    }

    @BeforeEach
    void setup() {
        shippingInstructionRequest = jsonTestUtility.getShippingInstructionRequest();
        shippingInstruction = jsonTestUtility.getShippingInstruction();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
    }


    @Test
    void testCreate() throws RunnerException {
        ShippingInstructionRequest request = new ShippingInstructionRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        ShippingInstruction mockShippingInstruction = ShippingInstruction.builder().build();
        mockShippingInstruction.setId(1L);
        ShippingInstructionResponse shippingInstructionResponse = objectMapper.convertValue(mockShippingInstruction, ShippingInstructionResponse.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(ShippingInstruction.class))).thenReturn(new ShippingInstruction());
        when(shippingInstructionDao.save(any())).thenReturn(mockShippingInstruction);
        when(jsonHelper.convertValue(any(), eq(ShippingInstructionResponse.class))).thenReturn(shippingInstructionResponse);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shippingInstructionService.create(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(shippingInstructionResponse), httpResponse);
    }

    @Test
    void testDelete() {
        // Mock
        when(shippingInstructionDao.findById(anyLong())).thenReturn(Optional.of(shippingInstruction));
        doNothing().when(shippingInstructionDao).delete(any());
        var responseEntity = shippingInstructionService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDelete2() {
        var responseEntity = shippingInstructionService.delete(CommonRequestModel.buildRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete3() {
        var responseEntity = shippingInstructionService.delete(CommonRequestModel.buildDependentDataRequest(CommonGetRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete5() {
        // Mock
        when(shippingInstructionDao.findById(anyLong())).thenThrow(new RuntimeException());
        var responseEntity = shippingInstructionService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete6() {
        // Mock
        when(shippingInstructionDao.findById(anyLong())).thenReturn(Optional.empty());
        var responseEntity = shippingInstructionService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCreateWithNullException() throws RunnerException {
        // Test
        Throwable t = assertThrows(Throwable.class, () -> shippingInstructionService.create(CommonRequestModel.buildRequest()));
        // Assert
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());

    }

    @Test
    void testListWithEmptyRequest() {
        // Test
        var responseEntity = shippingInstructionService.list(CommonRequestModel.buildRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithEmptyException() {
        when(shippingInstructionDao.findAll(any(), any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = shippingInstructionService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithNoResult() {
        when(shippingInstructionDao.findAll(any(), any())).thenReturn(Page.empty());
        // Test
        var responseEntity = shippingInstructionService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertTrue(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testListWithSuccessResult() {
        when(shippingInstructionDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shippingInstruction)));
        when(modelMapper.map(any(), eq(ShippingInstructionResponse.class))).thenReturn(objectMapper.convertValue(shippingInstruction, ShippingInstructionResponse.class));
        // Test
        var responseEntity = shippingInstructionService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertFalse(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testDeleteWithEmptyRequest() {
        when(shippingInstructionDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shippingInstruction)));
        when(modelMapper.map(any(), eq(ShippingInstructionResponse.class))).thenReturn(objectMapper.convertValue(shippingInstruction, ShippingInstructionResponse.class));
        // Test
        var responseEntity = shippingInstructionService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertFalse(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testRetrieveByIdWithEmptyRequest() {
        var responseEntity = shippingInstructionService.retrieveById(CommonRequestModel.buildRequest());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithEmptyIdRequest() {
        var responseEntity = shippingInstructionService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithIdNotPresent() {
        when(shippingInstructionDao.findById(anyLong())).thenReturn(Optional.empty());
        var responseEntity = shippingInstructionService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithExceptionInFindById() {
        when(shippingInstructionDao.findById(anyLong())).thenThrow(new RuntimeException());
        var responseEntity = shippingInstructionService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithSuccessResponse() {
        when(shippingInstructionDao.findById(anyLong())).thenReturn(Optional.of(shippingInstruction));
        when(jsonHelper.convertValue(any(), eq(ShippingInstructionResponse.class))).thenReturn(objectMapper.convertValue(shippingInstruction, ShippingInstructionResponse.class));
        var responseEntity = shippingInstructionService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByGUIdWithSuccessResponse() {
        when(shippingInstructionDao.findByGuid(any())).thenReturn(Optional.of(shippingInstruction));
        when(jsonHelper.convertValue(any(), eq(ShippingInstructionResponse.class))).thenReturn(objectMapper.convertValue(shippingInstruction, ShippingInstructionResponse.class));
        var responseEntity = shippingInstructionService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().guid("69281519-f079-4dfc-99fd-970c40bd14a9").build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreateWithCompletePayload() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        var inputShippingInstruction = shippingInstruction;
        ShippingInstructionRequest request = objectMapper.convertValue(inputShippingInstruction, ShippingInstructionRequest.class);
        ShippingInstructionResponse shippingInstructionResponse = objectMapper.convertValue(inputShippingInstruction, ShippingInstructionResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(ShippingInstruction.class))).thenReturn(inputShippingInstruction);
        when(shippingInstructionDao.save(any())).thenReturn(inputShippingInstruction);
        when(jsonHelper.convertValue(any(), eq(ShippingInstructionResponse.class))).thenReturn(shippingInstructionResponse);
        doThrow(new RuntimeException()).when(auditLogService).addAuditLog(any());
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shippingInstructionService.create(CommonRequestModel.buildRequest(request));
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testUpdateWithCompletePayload() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        var inputShippingInstruction = shippingInstruction;

        ShippingInstructionRequest request = objectMapper.convertValue(inputShippingInstruction, ShippingInstructionRequest.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(ShippingInstruction.class))).thenReturn(inputShippingInstruction);
        when(shippingInstructionDao.save(any())).thenReturn(inputShippingInstruction);
        when(shippingInstructionDao.findById(any())).thenReturn(Optional.of(inputShippingInstruction));
        doThrow(new RuntimeException()).when(auditLogService).addAuditLog(any());
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shippingInstructionService.update(CommonRequestModel.buildRequest(request));
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testShippingInstructionUpdateNullRequest() {
        // test
        var t = assertThrows(Throwable.class, () -> shippingInstructionService.update(CommonRequestModel.builder().build()));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testShippingInstructionUpdateNullRequestId() {
        // test
        var t = assertThrows(Throwable.class, () -> shippingInstructionService.update(CommonRequestModel.builder().data(new ShippingInstructionRequest()).build()));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testShippingInstructionUpdateWithIdDoesNotExists() {
        // mock
        when(shippingInstructionDao.findById(any())).thenReturn(Optional.empty());
        // test
        var t = assertThrows(Throwable.class, () -> shippingInstructionService.update(CommonRequestModel.builder().data(shippingInstructionRequest).build()));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, t.getMessage());
    }
}
