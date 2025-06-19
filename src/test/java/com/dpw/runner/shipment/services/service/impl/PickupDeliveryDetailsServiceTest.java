package com.dpw.runner.shipment.services.service.impl;

import com.dpw.api.commons.base.JsonAdaptor;
import com.dpw.runner.shipment.services.ReportingService.Models.DocumentRequest;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.ApiError;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.RAKCDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IKafkaAsyncService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.TransportInstructionValidationUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ExecutorService;

import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PickupDeliveryDetailsServiceTest {

    @Mock
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private AuditLogService auditLogService;

    @Mock
    private IKafkaAsyncService kafkaAsyncService;

    @Mock
    private IV1Service v1Service;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private ExecutorService executorService;

    @InjectMocks
    private PickupDeliveryDetailsService pickupDeliveryDetailsService;

    @Mock
    private TransportInstructionValidationUtil transportInstructionValidationUtil;

    @Mock
    private IShipmentServiceV3 shipmentServiceV3;

    @Mock
    private CommonUtils commonUtils;

    @Mock
    private IPartiesDao partiesDao;

    @Mock
    private IContainerV3Service containerV3Service;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        UserContext.setUser(UsersDto.builder().TenantId(1).Username("test").build());
    }

    @Test
    void testCreate_Success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        entity.setShipmentId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(pickupDeliveryDetailsDao.save(any(PickupDeliveryDetails.class))).thenReturn(entity);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetails.class))).thenReturn(entity);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(new PickupDeliveryDetailsResponse());
        when(pickupDeliveryDetailsDao.findByShipmentId(anyLong())).thenReturn(List.of(entity));
        doNothing().when(kafkaAsyncService).pushToKafkaTI(anyList(), anyBoolean(), anyLong());
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.create(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).save(any(PickupDeliveryDetails.class));
        verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
    }

    @Test
    void testCreate_failed_nullRequest() {
        PickupDeliveryDetailsRequest request = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.create(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testCreate_Exception() {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetails.class))).thenReturn(new PickupDeliveryDetails());
        when(pickupDeliveryDetailsDao.save(any(PickupDeliveryDetails.class))).thenThrow(new RuntimeException("Error"));

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.create(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).save(any(PickupDeliveryDetails.class));
    }

    @Test
    void testUpdate_Success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();
        request.setId(1L);
        PickupDeliveryDetails oldEntity = new PickupDeliveryDetails();
        PickupDeliveryDetails newEntity = new PickupDeliveryDetails();
        newEntity.setId(1L);
        newEntity.setShipmentId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.of(oldEntity));
        when(pickupDeliveryDetailsDao.save(any(PickupDeliveryDetails.class))).thenReturn(newEntity);
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(jsonHelper.readFromJson(anyString(), eq(PickupDeliveryDetails.class))).thenReturn(oldEntity);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetails.class))).thenReturn(newEntity);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(new PickupDeliveryDetailsResponse());
        when(pickupDeliveryDetailsDao.findByShipmentId(anyLong())).thenReturn(List.of(newEntity));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        doNothing().when(kafkaAsyncService).pushToKafkaTI(anyList(), anyBoolean(), anyLong());
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.update(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findById(anyLong());
        verify(pickupDeliveryDetailsDao, times(1)).save(any(PickupDeliveryDetails.class));
        verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
    }

    @Test
    void testUpdate_Success_null_request() throws RunnerException {
        PickupDeliveryDetailsRequest request = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.update(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }


    @Test
    void testUpdate_Success_null_request_id() throws RunnerException {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();
        request.setId(null);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.update(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testUpdate_Exception() throws RunnerException {
        PickupDeliveryDetailsRequest request = new PickupDeliveryDetailsRequest();
        request.setId(1L);
        PickupDeliveryDetails newEntity = new PickupDeliveryDetails();
        newEntity.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetails.class))).thenReturn(newEntity);

        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.of(new PickupDeliveryDetails()));
        when(pickupDeliveryDetailsDao.save(any(PickupDeliveryDetails.class))).thenThrow(new RuntimeException("Error"));

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.update(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findById(anyLong());
        verify(pickupDeliveryDetailsDao, times(1)).save(any(PickupDeliveryDetails.class));
    }

    @Test
    void testList_Success() {
        ListCommonRequest request = constructListCommonRequest("id" , 1 , "=");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        Page<PickupDeliveryDetails> page = new PageImpl<>(Collections.singletonList(entity), PageRequest.of(0, 10), 1);

        when(pickupDeliveryDetailsDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(new PickupDeliveryDetailsResponse());

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.list(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findAll(any(Specification.class), any(Pageable.class));
    }

    @Test
    void testList_Success1() {
        ListCommonRequest request = constructListCommonRequest("id" , 1 , "=");
        request.setPopulateRAKC(true);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        entity.setTransporterDetail(Parties.builder().orgId("1L").build());
        entity.setSourceDetail(Parties.builder().addressId("1L").build());
        entity.setDestinationDetail(Parties.builder().build());
        Page<PickupDeliveryDetails> page = new PageImpl<>(Collections.singletonList(entity), PageRequest.of(0, 10), 1);

        when(pickupDeliveryDetailsDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(new PickupDeliveryDetailsResponse());

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.list(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findAll(any(Specification.class), any(Pageable.class));
    }


    @Test
    void testList_Success2() {
        ListCommonRequest request = constructListCommonRequest("id" , 1 , "=");
        request.setPopulateRAKC(true);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        Parties parties = Parties.builder().orgId("1L").addressId("10").build();
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        entity.setTransporterDetail(parties);
        entity.setSourceDetail(parties);
        entity.setDestinationDetail(parties);
        Page<PickupDeliveryDetails> page = new PageImpl<>(Collections.singletonList(entity), PageRequest.of(0, 10), 1);
        List<RAKCDetailsResponse> rakcDetailsResponses = new ArrayList<>();
        rakcDetailsResponses.add(RAKCDetailsResponse.builder().id(10L).build());
        PartiesResponse partiesResponse = PartiesResponse.builder().orgId("1L").addressId("10").build();
        PickupDeliveryDetailsResponse pickupDeliveryDetailsResponse = new PickupDeliveryDetailsResponse();
        pickupDeliveryDetailsResponse.setId(1L);
        pickupDeliveryDetailsResponse.setTransporterDetail(partiesResponse);
        pickupDeliveryDetailsResponse.setSourceDetail(partiesResponse);
        pickupDeliveryDetailsResponse.setDestinationDetail(partiesResponse);


        when(pickupDeliveryDetailsDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(pickupDeliveryDetailsResponse);
        when(v1Service.addressList(any())).thenReturn(V1DataResponse.builder().entities(new Object()).build());
        when(jsonHelper.convertValueToList(any(), eq(RAKCDetailsResponse.class))).thenReturn(rakcDetailsResponses);

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.list(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findAll(any(Specification.class), any(Pageable.class));
    }

    @Test
    void testList_Success3() {
        ListCommonRequest request = constructListCommonRequest("id" , 1 , "=");
        request.setPopulateRAKC(true);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        Parties parties = Parties.builder().orgId("1L").addressId("10").build();
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        entity.setTransporterDetail(parties);
        entity.setSourceDetail(parties);
        entity.setIsDirectDelivery(true);
        Page<PickupDeliveryDetails> page = new PageImpl<>(Collections.singletonList(entity), PageRequest.of(0, 10), 1);
        List<RAKCDetailsResponse> rakcDetailsResponses = new ArrayList<>();
        rakcDetailsResponses.add(RAKCDetailsResponse.builder().id(10L).build());
        PartiesResponse partiesResponse = PartiesResponse.builder().orgId("1L").addressId("10").build();
        PickupDeliveryDetailsResponse pickupDeliveryDetailsResponse = new PickupDeliveryDetailsResponse();
        pickupDeliveryDetailsResponse.setId(1L);
        pickupDeliveryDetailsResponse.setTransporterDetail(partiesResponse);
        pickupDeliveryDetailsResponse.setSourceDetail(partiesResponse);
        pickupDeliveryDetailsResponse.setIsDirectDelivery(true);

        when(pickupDeliveryDetailsDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(pickupDeliveryDetailsResponse);
        when(v1Service.addressList(any())).thenReturn(V1DataResponse.builder().entities(new Object()).build());
        when(jsonHelper.convertValueToList(any(), eq(RAKCDetailsResponse.class))).thenReturn(rakcDetailsResponses);

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.list(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findAll(any(Specification.class), any(Pageable.class));
    }

    @Test
    void testList_Success4() {
        ListCommonRequest request = constructListCommonRequest("id" , 1 , "=");
        request.setPopulateRAKC(true);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        Parties parties = Parties.builder().orgId("1L").addressId("10").build();
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        entity.setTransporterDetail(parties);
        entity.setSourceDetail(parties);
        Page<PickupDeliveryDetails> page = new PageImpl<>(Collections.singletonList(entity), PageRequest.of(0, 10), 1);
        List<RAKCDetailsResponse> rakcDetailsResponses = new ArrayList<>();
        rakcDetailsResponses.add(RAKCDetailsResponse.builder().id(10L).build());
        PickupDeliveryDetailsResponse pickupDeliveryDetailsResponse = new PickupDeliveryDetailsResponse();
        pickupDeliveryDetailsResponse.setId(1L);
        pickupDeliveryDetailsResponse.setTransporterDetail(PartiesResponse.builder().orgId("1L").build());
        pickupDeliveryDetailsResponse.setSourceDetail(PartiesResponse.builder().addressId("10").build());

        when(pickupDeliveryDetailsDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(pickupDeliveryDetailsResponse);
        when(v1Service.addressList(any())).thenReturn(V1DataResponse.builder().entities(new Object()).build());
        when(jsonHelper.convertValueToList(any(), eq(RAKCDetailsResponse.class))).thenReturn(rakcDetailsResponses);

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.list(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findAll(any(Specification.class), any(Pageable.class));
    }

    @Test
    void testList_Exception() {
        ListCommonRequest request = constructListCommonRequest("id" , 1 , "=");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);

        when(pickupDeliveryDetailsDao.findAll(any(Specification.class), any(Pageable.class))).thenThrow(new RuntimeException("Error"));

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.list(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findAll(any(Specification.class), any(Pageable.class));
    }

    @Test
    void testDelete_Success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        entity.setShipmentId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.of(entity));
        when(pickupDeliveryDetailsDao.findByShipmentId(anyLong())).thenReturn(new ArrayList<>());

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.delete(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findById(anyLong());
        verify(pickupDeliveryDetailsDao, times(1)).delete(any(PickupDeliveryDetails.class));
        verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
    }

    @Test
    void testDelete_Exception() {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.of(new PickupDeliveryDetails()));
        doThrow(new RuntimeException("Error")).when(pickupDeliveryDetailsDao).delete(any(PickupDeliveryDetails.class));

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.delete(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findById(anyLong());
        verify(pickupDeliveryDetailsDao, times(1)).delete(any(PickupDeliveryDetails.class));
    }

    @Test
    void testRetrieveById_Success() {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        PickupDeliveryDetails entity = new PickupDeliveryDetails();
        entity.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.of(entity));
        when(jsonHelper.convertValue(any(), eq(PickupDeliveryDetailsResponse.class))).thenReturn(new PickupDeliveryDetailsResponse());

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.retrieveById(commonRequestModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(pickupDeliveryDetailsDao, times(1)).findById(anyLong());
    }

    @Test
    void testRetrieveById_Exception() {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.retrieveById(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());


        verify(pickupDeliveryDetailsDao, times(1)).findById(anyLong());
    }

    @Test
    void testRetrieveById_NullRequest() {
        CommonGetRequest request = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.retrieveById(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testRetrieveById_NullId() {
        CommonGetRequest request = CommonGetRequest.builder().id(null).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.retrieveById(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testDelete_NullId() {
        CommonGetRequest request = CommonGetRequest.builder().id(null).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.delete(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testDelete_NullRequest() {
        CommonGetRequest request = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.delete(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }
    @Test
    void testCreate2() {
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.dataList(Mockito.<List<IRunnerRequest>>any()))
                .thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder2 = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder2.data(Mockito.<IRunnerRequest>any())).thenReturn(commonRequestModelBuilder);
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder2.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();
        ResponseEntity<IRunnerResponse> actualCreateResult = pickupDeliveryDetailsService.create(commonRequestModel);
        verify(commonRequestModelBuilder2).data(Mockito.<IRunnerRequest>any());
        verify(commonRequestModelBuilder).dataList(Mockito.<List<IRunnerRequest>>any());
        ApiError error = ((RunnerResponse<Object>) actualCreateResult.getBody()).getError();
        assertEquals("Request is empty for Pickup Delivery create with Request Id null", error.getMessage());
        assertNull(((RunnerResponse<Object>) actualCreateResult.getBody()).getData());
        assertNull(((RunnerResponse<Object>) actualCreateResult.getBody()).getRequestId());
        assertNull(((RunnerResponse<Object>) actualCreateResult.getBody()).getWarning());
        assertNull(error.getErrors());
        assertEquals(0, ((RunnerResponse<Object>) actualCreateResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) actualCreateResult.getBody()).getCount());
        assertEquals(HttpStatus.BAD_REQUEST, error.getStatus());
        assertEquals(HttpStatus.BAD_REQUEST, actualCreateResult.getStatusCode());
        assertFalse(((RunnerResponse<Object>) actualCreateResult.getBody()).isSuccess());
        assertTrue(actualCreateResult.hasBody());
        assertTrue(actualCreateResult.getHeaders().isEmpty());
    }
    @Test
    void testCreateV2() {
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();
        ResponseEntity<IRunnerResponse> actualCreateV2Result = pickupDeliveryDetailsService.createV2(commonRequestModel);
        verify(commonRequestModelBuilder).data(Mockito.<IRunnerRequest>any());
        ApiError error = ((RunnerResponse<Object>) actualCreateV2Result.getBody()).getError();
        assertEquals("Request is empty for Pickup Delivery create with Request Id null", error.getMessage());
        assertNull(((RunnerResponse<Object>) actualCreateV2Result.getBody()).getData());
        assertNull(((RunnerResponse<Object>) actualCreateV2Result.getBody()).getRequestId());
        assertNull(((RunnerResponse<Object>) actualCreateV2Result.getBody()).getWarning());
        assertNull(error.getErrors());
        assertEquals(0, ((RunnerResponse<Object>) actualCreateV2Result.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) actualCreateV2Result.getBody()).getCount());
        assertEquals(HttpStatus.BAD_REQUEST, error.getStatus());
        assertEquals(HttpStatus.BAD_REQUEST, actualCreateV2Result.getStatusCode());
        assertFalse(((RunnerResponse<Object>) actualCreateV2Result.getBody()).isSuccess());
        assertTrue(actualCreateV2Result.hasBody());
        assertTrue(actualCreateV2Result.getHeaders().isEmpty());
    }
    @Test
    void testCreateTransportInstruction() {
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();
        ResponseEntity<IRunnerResponse> actualCreateTransportInstructionResult = pickupDeliveryDetailsService
                .createTransportInstruction(commonRequestModel);
        verify(commonRequestModelBuilder).data(Mockito.<IRunnerRequest>any());
        ApiError error = ((RunnerResponse<Object>) actualCreateTransportInstructionResult.getBody()).getError();
        assertEquals("Request is empty for Pickup Delivery create with Request Id null", error.getMessage());
        assertNull(((RunnerResponse<Object>) actualCreateTransportInstructionResult.getBody()).getData());
        assertNull(((RunnerResponse<Object>) actualCreateTransportInstructionResult.getBody()).getRequestId());
        assertNull(((RunnerResponse<Object>) actualCreateTransportInstructionResult.getBody()).getWarning());
        assertNull(error.getErrors());
        assertEquals(0, ((RunnerResponse<Object>) actualCreateTransportInstructionResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) actualCreateTransportInstructionResult.getBody()).getCount());
        assertEquals(HttpStatus.BAD_REQUEST, error.getStatus());
        assertEquals(HttpStatus.BAD_REQUEST, actualCreateTransportInstructionResult.getStatusCode());
        assertFalse(((RunnerResponse<Object>) actualCreateTransportInstructionResult.getBody()).isSuccess());
        assertTrue(actualCreateTransportInstructionResult.hasBody());
        assertTrue(actualCreateTransportInstructionResult.getHeaders().isEmpty());
    }

    /**
     * Method under test:
     * {@link PickupDeliveryDetailsService#createTransportInstruction(CommonRequestModel)}
     */
    @Test
    void testCreateTransportInstruction2() throws IOException {
        PickupDeliveryDetailsRequest pickupDeliveryDetailsRequest = getPickupDeliveryRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(pickupDeliveryDetailsRequest).build();
        when(transportInstructionValidationUtil.validateShipmentId(any(PickupDeliveryDetailsRequest.class))).thenThrow(new ValidationException("Error"));
        assertThrows(ValidationException.class,()->pickupDeliveryDetailsService
                .createV2(commonRequestModel));
    }
    @Test
    void testCreateTransportInstruction3() throws IOException {
        PickupDeliveryDetailsRequest pickupDeliveryDetailsRequest = getPickupDeliveryRequest();
        ModelMapper modelMapper = new ModelMapper();
        PickupDeliveryDetails pickupDeliveryDetails = modelMapper.map(pickupDeliveryDetailsRequest, PickupDeliveryDetails.class);
        pickupDeliveryDetails.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(pickupDeliveryDetailsRequest).build();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SHP001");
        ContainerNumberCheckResponse containerNumberCheckResponse = new ContainerNumberCheckResponse();
        containerNumberCheckResponse.setSuccess(true);
        when(containerV3Service.validateContainerNumber(eq("CONT1234567"))).thenReturn(containerNumberCheckResponse);
        when(transportInstructionValidationUtil.validateShipmentId(any())).thenReturn(shipmentDetails);
        when(jsonHelper.convertValue(pickupDeliveryDetailsRequest, PickupDeliveryDetails.class)).thenReturn(pickupDeliveryDetails);
        when(pickupDeliveryDetailsDao.save(pickupDeliveryDetails)).thenReturn(pickupDeliveryDetails);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.createV2(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }
    @Test
    void testUpdateTransportInstruction() throws IOException, RunnerException {
        PickupDeliveryDetailsRequest pickupDeliveryDetailsRequest = getPickupDeliveryRequest();
        ModelMapper modelMapper = new ModelMapper();
        PickupDeliveryDetails pickupDeliveryDetails = modelMapper.map(pickupDeliveryDetailsRequest, PickupDeliveryDetails.class);
        pickupDeliveryDetails.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(pickupDeliveryDetailsRequest).build();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentId("SHP001");
        ContainerNumberCheckResponse containerNumberCheckResponse = new ContainerNumberCheckResponse();
        containerNumberCheckResponse.setSuccess(true);
        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.of(pickupDeliveryDetails));
        when(containerV3Service.validateContainerNumber(eq("CONT1234567"))).thenReturn(containerNumberCheckResponse);
        when(transportInstructionValidationUtil.validateShipmentId(any())).thenReturn(shipmentDetails);
        when(jsonHelper.convertValue(pickupDeliveryDetailsRequest, PickupDeliveryDetails.class)).thenReturn(pickupDeliveryDetails);
        when(pickupDeliveryDetailsDao.save(pickupDeliveryDetails)).thenReturn(pickupDeliveryDetails);
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.updateV2(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }
    @Test
    void testUpdateTransportInstruction2() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.updateV2(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }
    @Test
    void testUpdateTransportInstruction3() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(new PickupDeliveryDetailsRequest()).build();
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.updateV2(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }
    @Test
    void testUpdateTransportInstruction4() throws RunnerException {
        PickupDeliveryDetailsRequest pickupDeliveryDetailsRequest = new PickupDeliveryDetailsRequest();
        pickupDeliveryDetailsRequest.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(pickupDeliveryDetailsRequest).build();
        ResponseEntity<IRunnerResponse> response = pickupDeliveryDetailsService.updateV2(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }
    @Test
    void testUpdateTransportInstruction5() {
        PickupDeliveryDetailsRequest pickupDeliveryDetailsRequest = new PickupDeliveryDetailsRequest();
        pickupDeliveryDetailsRequest.setId(1L);
        pickupDeliveryDetailsRequest.setTiReferenceNumber("SHP001-1");

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(pickupDeliveryDetailsRequest).build();
        when(pickupDeliveryDetailsDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class,()-> pickupDeliveryDetailsService.updateV2(commonRequestModel));
    }
    private PickupDeliveryDetailsRequest getPickupDeliveryRequest() throws IOException {
        String request = "{\n" +
                "  \"actualDelivery\": \"2025-06-16T09:17:02.274Z\",\n" +
                "    \"tiReferenceNumber\": \"SHP001-1\",\n" +
                "  \"actualPickup\": \"2025-06-16T08:30:00.000Z\",\n" +
                "  \"actualPickupOrDelivery\": \"2025-06-16T08:30:00.000Z\",\n" +
                "  \"agentDetail\": {\n" +
                "    \"addressCode\": \"AG001\",\n" +
                "    \"addressData\": {},\n" +
                "    \"addressId\": \"ADDR-AG001\",\n" +
                "    \"countryCode\": \"IN\",\n" +
                "    \"entityId\": 101,\n" +
                "    \"entityType\": \"AGENT\",\n" +
                "    \"guid\": \"cfe15c9a-3a91-4f30-8cc4-0dcb143abb77\",\n" +
                "    \"id\": 101,\n" +
                "    \"isAddressFreeText\": false,\n" +
                "    \"orgCode\": \"ORG-AG1\",\n" +
                "    \"orgData\": {},\n" +
                "    \"orgId\": \"ORGID-AG001\",\n" +
                "    \"tenantId\": 1,\n" +
                "    \"type\": \"Agent\"\n" +
                "  },\n" +
                "  \"brokerDetail\": {\n" +
                "    \"addressCode\": \"BR001\",\n" +
                "    \"addressData\": {},\n" +
                "    \"addressId\": \"ADDR-BR001\",\n" +
                "    \"countryCode\": \"US\",\n" +
                "    \"entityId\": 102,\n" +
                "    \"entityType\": \"BROKER\",\n" +
                "    \"guid\": \"587c8d11-d6c1-476c-90c6-c0a8573de2c0\",\n" +
                "    \"id\": 102,\n" +
                "    \"isAddressFreeText\": false,\n" +
                "    \"orgCode\": \"ORG-BR1\",\n" +
                "    \"orgData\": {},\n" +
                "    \"orgId\": \"ORGID-BR001\",\n" +
                "    \"tenantId\": 1,\n" +
                "    \"type\": \"Broker\"\n" +
                "  },\n" +
                "  \"deliveryGateIn\": \"2025-06-16T07:00:00.000Z\",\n" +
                "  \"deliveryGateOut\": \"2025-06-16T09:00:00.000Z\",\n" +
                "  \"destinationDetail\": {\n" +
                "    \"addressCode\": \"DST001\",\n" +
                "    \"addressData\": {},\n" +
                "    \"addressId\": \"ADDR-DST001\",\n" +
                "    \"countryCode\": \"AE\",\n" +
                "    \"entityId\": 103,\n" +
                "    \"entityType\": \"DESTINATION\",\n" +
                "    \"guid\": \"b4dc262e-9dd7-44cd-86f1-2f108c3e5ba3\",\n" +
                "    \"id\": 103,\n" +
                "    \"isAddressFreeText\": false,\n" +
                "    \"orgCode\": \"ORG-DST1\",\n" +
                "    \"orgData\": {},\n" +
                "    \"orgId\": \"ORGID-DST001\",\n" +
                "    \"tenantId\": 1,\n" +
                "    \"type\": \"Destination\"\n" +
                "  },\n" +
                "  \"dropMode\": \"Direct\",\n" +
                "  \"emptyTruckInDate\": \"2025-06-16T06:00:00.000Z\",\n" +
                "  \"estimatedDelivery\": \"2025-06-16T09:00:00.000Z\",\n" +
                "  \"estimatedPickup\": \"2025-06-16T08:00:00.000Z\",\n" +
                "  \"estimatedPickupOrDelivery\": \"2025-06-16T08:00:00.000Z\",\n" +
                "  \"fclAvailableDate\": \"2025-06-15T16:00:00.000Z\",\n" +
                "  \"guid\": \"4a42c25d-f8a4-4069-8c3c-1686b0b5b78a\",\n" +
                "  \"id\": 999,\n" +
                "  \"interimReceipt\": \"IR123456\",\n" +
                "  \"isDirectDelivery\": true,\n" +
                "  \"labourCharge\": 200,\n" +
                "  \"labourChargeUnit\": \"INR\",\n" +
                "  \"labourDuration\": {\n" +
                "    \"hour\": 2,\n" +
                "    \"minute\": 0,\n" +
                "    \"nano\": 0,\n" +
                "    \"second\": 0\n" +
                "  },\n" +
                "  \"loadedTruckGateOutDate\": \"2025-06-16T07:30:00.000Z\",\n" +
                "  \"partiesList\": [\n" +
                "    {\n" +
                "      \"addressCode\": \"PTY001\",\n" +
                "      \"addressData\": {},\n" +
                "      \"addressId\": \"ADDR-PTY001\",\n" +
                "      \"countryCode\": \"IN\",\n" +
                "      \"createdAt\": \"2025-06-01T09:17:02.274Z\",\n" +
                "      \"createdBy\": \"admin\",\n" +
                "      \"entityId\": 104,\n" +
                "      \"entityType\": \"PARTY\",\n" +
                "      \"guid\": \"4f5b8c80-1ed9-4f7a-809f-b978c82d2ae1\",\n" +
                "      \"id\": 104,\n" +
                "      \"isAddressFreeText\": false,\n" +
                "      \"isDeleted\": false,\n" +
                "      \"orgCode\": \"ORG-PTY1\",\n" +
                "      \"orgData\": {},\n" +
                "      \"orgId\": \"ORGID-PTY001\",\n" +
                "      \"tenantId\": 1,\n" +
                "      \"type\": \"Party\",\n" +
                "      \"updatedAt\": \"2025-06-10T09:17:02.274Z\",\n" +
                "      \"updatedBy\": \"admin\"\n" +
                "    }\n" +
                "  ],\n" +
                "  \"pickupDeliveryInstruction\": \"Handle with care\",\n" +
                "  \"pickupGateIn\": \"2025-06-16T06:30:00.000Z\",\n" +
                "  \"pickupGateOut\": \"2025-06-16T07:30:00.000Z\",\n" +
                "  \"pickupOrDelivery\": \"2025-06-16T08:00:00.000Z\",\n" +
                "  \"portTransportAdvised\": \"2025-06-15T18:00:00.000Z\",\n" +
                "  \"remarks\": \"All operations smooth.\",\n" +
                "  \"requiredBy\": \"2025-06-17T12:00:00.000Z\",\n" +
                "  \"shipmentId\": 123456,\n" +
                "  \"shipperRef\": \"SHIPREF1234\",\n" +
                "  \"sourceDetail\": {\n" +
                "    \"addressCode\": \"SRC001\",\n" +
                "    \"addressData\": {},\n" +
                "    \"addressId\": \"ADDR-SRC001\",\n" +
                "    \"countryCode\": \"SG\",\n" +
                "    \"entityId\": 105,\n" +
                "    \"entityType\": \"SOURCE\",\n" +
                "    \"guid\": \"99c6bb5c-f57e-4a8b-8d9f-86b2f507310b\",\n" +
                "    \"id\": 105,\n" +
                "    \"isAddressFreeText\": false,\n" +
                "    \"orgCode\": \"ORG-SRC1\",\n" +
                "    \"orgData\": {},\n" +
                "    \"orgId\": \"ORGID-SRC001\",\n" +
                "    \"tenantId\": 1,\n" +
                "    \"type\": \"Source\"\n" +
                "  },\n" +
                "  \"storageCharge\": 100,\n" +
                "  \"storageChargeDuration\": \"2025-06-16T09:00:00.000Z\",\n" +
                "  \"storageChargeUnit\": \"INR\",\n" +
                "  \"storageDate\": \"2025-06-15T10:00:00.000Z\",\n" +
                "  \"tiLegsList\": [\n" +
                "    {\n" +
                "      \"actualDelivery\": \"2025-06-16T09:00:00.000Z\",\n" +
                "      \"actualPickup\": \"2025-06-16T08:00:00.000Z\",\n" +
                "      \"destination\": {\n" +
                "        \"addressCode\": \"DST001\",\n" +
                "       \"addressData\": {\n" +
                "                \"Id\": 154728,\n" +
                "                \"Guid\": \"2ffd6f36-a757-42a0-8fa2-4b5c6f74a345\",\n" +
                "                \"AddressShortCode\": \"FRDO0007557\",\n" +
                "                \"CompanyName\": \"ietnam\",\n" +
                "                \"AddressType\": 2,\n" +
                "                \"Address1\": \"sdjhsgaf\",\n" +
                "                \"Address2\": \"ADDAS\",\n" +
                "                \"Country\": \"VNM\",\n" +
                "                \"City\": \"Ho Chi Minh City\",\n" +
                "                \"State\": \"SG\",\n" +
                "                \"ZipPostCode\": \"123123\",\n" +
                "                \"Email\": \"ashish@dpworld.com\",\n" +
                "                \"OrgGuid\": \"db5f7a00-8513-47cc-9d31-72ad3a138b8c\",\n" +
                "                \"OrgOrganizationCode\": \"FRDC0005613\",\n" +
                "                \"OrgSource\": \"CRP\",\n" +
                "                \"OrgFullName\": \" ietnam1&\",\n" +
                "                \"OrgActiveClient\": true,\n" +
                "                \"OrgReceivables\": false,\n" +
                "                \"OrgPayables\": false,\n" +
                "                \"RegulatedAgent\": false,\n" +
                "                \"KnownConsignor\": false\n" +
                "            },\n" +
                "        \"addressId\": \"ADDR-DST001\",\n" +
                "        \"countryCode\": \"AE\",\n" +
                "        \"entityId\": 106,\n" +
                "        \"entityType\": \"DEST\",\n" +
                "        \"guid\": \"a4be51c0-46b0-4a4d-b4c6-3ff8686494f7\",\n" +
                "        \"id\": 106,\n" +
                "        \"isAddressFreeText\": false,\n" +
                "        \"orgCode\": \"ORG-DST1\",\n" +
                "        \"orgData\": {\n" +
                "                \"Id\": 2551225,\n" +
                "                \"Guid\": \"db5f7a00-8513-47cc-9d31-72ad3a138b8c\",\n" +
                "                \"OrganizationCode\": \"FRDC0005613\",\n" +
                "                \"FullName\": \" ietnam1&\",\n" +
                "                \"Address1\": \"sdjhsgaf\",\n" +
                "                \"Country\": \"VNM\",\n" +
                "                \"City\": \"Ho Chi Minh City\",\n" +
                "                \"State\": \"SG\",\n" +
                "                \"Email\": \"ashish@dpworld.com\",\n" +
                "                \"ForworderAgent\": false,\n" +
                "                \"Receivables\": false,\n" +
                "                \"Payables\": false,\n" +
                "                \"CompanyId\": 1,\n" +
                "                \"InsertUserIdUsername\": \"EGYPQAP100ALEX@dpworld.com\",\n" +
                "                \"UpdateUserIdUsername\": \"nga.nguyen1@dpworld.com\",\n" +
                "                \"label\": \" ietnam1&\",\n" +
                "                \"value\": \"FRDC0005613\"\n" +
                "            },\n" +
                "        \"orgId\": \"ORGID-DST001\",\n" +
                "        \"tenantId\": 1,\n" +
                "        \"type\": \"Destination\"\n" +
                "      },\n" +
                "      \"dropMode\": \"Warehouse\",\n" +
                "      \"estimatedDelivery\": \"2025-06-16T09:00:00.000Z\",\n" +
                "      \"estimatedPickup\": \"2025-06-16T08:00:00.000Z\",\n" +
                "      \"guid\": \"7e1b2552-2046-4de2-a8e3-49b00e623328\",\n" +
                "      \"id\": 201,\n" +
                "      \"legType\": \"Truck\",\n" +
                "      \"origin\": {\n" +
                "        \"addressCode\": \"SRC001\",\n" +
                "        \"addressData\": {\n" +
                "                \"Id\": 154728,\n" +
                "                \"Guid\": \"2ffd6f36-a757-42a0-8fa2-4b5c6f74a345\",\n" +
                "                \"AddressShortCode\": \"FRDO0007557\",\n" +
                "                \"CompanyName\": \"ietnam\",\n" +
                "                \"AddressType\": 2,\n" +
                "                \"Address1\": \"sdjhsgaf\",\n" +
                "                \"Address2\": \"ADDAS\",\n" +
                "                \"Country\": \"VNM\",\n" +
                "                \"City\": \"Ho Chi Minh City\",\n" +
                "                \"State\": \"SG\",\n" +
                "                \"ZipPostCode\": \"123123\",\n" +
                "                \"Email\": \"ashish@dpworld.com\",\n" +
                "                \"OrgGuid\": \"db5f7a00-8513-47cc-9d31-72ad3a138b8c\",\n" +
                "                \"OrgOrganizationCode\": \"FRDC0005613\",\n" +
                "                \"OrgSource\": \"CRP\",\n" +
                "                \"OrgFullName\": \" ietnam1&\",\n" +
                "                \"OrgActiveClient\": true,\n" +
                "                \"OrgReceivables\": false,\n" +
                "                \"OrgPayables\": false,\n" +
                "                \"RegulatedAgent\": false,\n" +
                "                \"KnownConsignor\": false\n" +
                "            },\n" +
                "        \"addressId\": \"ADDR-SRC001\",\n" +
                "        \"countryCode\": \"SG\",\n" +
                "        \"entityId\": 105,\n" +
                "        \"entityType\": \"SOURCE\",\n" +
                "        \"guid\": \"99c6bb5c-f57e-4a8b-8d9f-86b2f507310b\",\n" +
                "        \"id\": 105,\n" +
                "        \"isAddressFreeText\": false,\n" +
                "        \"orgCode\": \"ORG-SRC1\",\n" +
                "        \"orgData\": {\n" +
                "                \"Id\": 2551225,\n" +
                "                \"Guid\": \"db5f7a00-8513-47cc-9d31-72ad3a138b8c\",\n" +
                "                \"OrganizationCode\": \"FRDC0005613\",\n" +
                "                \"FullName\": \" ietnam1&\",\n" +
                "                \"Address1\": \"sdjhsgaf\",\n" +
                "                \"Country\": \"VNM\",\n" +
                "                \"City\": \"Ho Chi Minh City\",\n" +
                "                \"State\": \"SG\",\n" +
                "                \"Email\": \"ashish@dpworld.com\",\n" +
                "                \"ForworderAgent\": false,\n" +
                "                \"Receivables\": false,\n" +
                "                \"Payables\": false,\n" +
                "                \"CompanyId\": 1,\n" +
                "                \"InsertUserIdUsername\": \"EGYPQAP100ALEX@dpworld.com\",\n" +
                "                \"UpdateUserIdUsername\": \"nga.nguyen1@dpworld.com\",\n" +
                "                \"label\": \" ietnam1&\",\n" +
                "                \"value\": \"FRDC0005613\"\n" +
                "            },\n" +
                "        \"orgId\": \"ORGID-SRC001\",\n" +
                "        \"tenantId\": 1,\n" +
                "        \"type\": \"Source\"\n" +
                "      },\n" +
                "      \"remarks\": \"Smooth transit\",\n" +
                "      \"requiredBy\": \"2025-06-17T09:00:00.000Z\",\n" +
                "      \"sequence\": 1,\n" +
                "      \"tiContainers\": [\n" +
                "  {\n" +
                "    \"number\": \"CONT1234567\",\n" +
                "    \"containerSize\": \"40FT\",\n" +
                "    \"containerType\": \"Dry\",\n" +
                "    \"sealNumber\": \"SEAL7890\",\n" +
                "    \"isHazardous\": false,\n" +
                "    \"grossWeight\": 24500,\n" +
                "    \"grossWeightUnit\": \"KG\",\n" +
                "    \"volume\": 500,\n" +
                "    \"volumeUnit\": \"KG\",\n" +
                "    \"netWeight\": 500,\n" +
                "    \"netWeightUnit\": \"KG\",\n" +
                "    \"weightUnit\": \"KG\",\n" +
                "    \"type\": \"STANDARD\", \n" +
                "    \"guid\": \"d1a7337f-6b2e-4cd5-9877-3f0b12b9a6bc\",\n" +
                "    \"id\": 401,\n" +
                "    \"tiLegId\": 201\n" +
                "  }\n" +
                "],\n" +
                "     \"tiPackages\": [\n" +
                "  {\n" +
                "    \"packageType\": \"BOX\",\n" +
                "    \"numberOfPackages\": 50,\n" +
                "    \"grossWeight\": 500,\n" +
                "    \"grossWeightUnit\": \"KG\",\n" +
                "    \"volume\": 500,\n" +
                "    \"volumeUnit\": \"KG\",\n" +
                "    \"netWeight\": 500,\n" +
                "    \"netWeightUnit\": \"KG\",\n" +
                "    \"isHazardous\": true,\n" +
                "    \"dimensions\": \"10x20x30\",\n" +
                "    \"hazardousDetails\": \"Contains Lithium Batteries\",\n" +
                "    \"guid\": \"e1c4e7e1-bcdc-4e6b-bb38-ccc91172be47\",\n" +
                "    \"id\": 501,\n" +
                "    \"tiLegId\": 201\n" +
                "  }\n" +
                "],\n" +
                "      \"tiReferences\": [\n" +
                "  {\n" +
                "    \"type\": \"BOOKING_REFERENCE\",\n" +
                "    \"reference\": \"BOOK123456\",\n" +
                "    \"guid\": \"f57e2b8f-7c55-4201-a9f1-d810d356f1b2\",\n" +
                "    \"id\": 601,\n" +
                "    \"tiLegId\": 201\n" +
                "  },\n" +
                "  {\n" +
                "    \"type\": \"SHIPMENT_REFERENCE\",\n" +
                "    \"reference\": \"SHIP78910\",\n" +
                "    \"guid\": \"fb1237ea-2241-4ebf-8e34-bc02e7f4e06c\",\n" +
                "    \"id\": 602,\n" +
                "    \"tiLegId\": 201\n" +
                "  }\n" +
                "]\n,\n" +
                "      \"tiTruckDriverDetails\": [\n" +
                "        {\n" +
                "          \"driverMobileNumber\": \"9876543210\",\n" +
                "          \"driverName\": \"John Doe\",\n" +
                "          \"guid\": \"c6e1f72d-2480-4c7a-aacf-8d6f32b7cbb8\",\n" +
                "          \"id\": 301,\n" +
                "          \"tiLegId\": 201,\n" +
                "          \"trailerNumberPlate\": \"MH12AB1234\",\n" +
                "          \"truckNumberPlate\": \"MH12XY9876\",\n" +
                "          \"truckOrTrailerType\": \"Truck\"\n" +
                "        }\n" +
                "      ]\n" +
                "    }\n" +
                "  ],\n" +
                "  \"transporterDetail\": {\n" +
                "    \"addressCode\": \"TRANS001\",\n" +
                "    \"addressData\": {},\n" +
                "    \"addressId\": \"ADDR-TRANS001\",\n" +
                "    \"countryCode\": \"IN\",\n" +
                "    \"entityId\": 107,\n" +
                "    \"entityType\": \"TRANSPORTER\",\n" +
                "    \"guid\": \"2e0a44dc-81c0-464b-b25b-937f75e16452\",\n" +
                "    \"id\": 107,\n" +
                "    \"isAddressFreeText\": false,\n" +
                "    \"orgCode\": \"ORG-TRANS1\",\n" +
                "    \"orgData\": {},\n" +
                "    \"orgId\": \"ORGID-TRANS001\",\n" +
                "    \"tenantId\": 1,\n" +
                "    \"type\": \"Transporter\"\n" +
                "  },\n" +
                "  \"truckWaitDuration\": {\n" +
                "    \"hour\": 1,\n" +
                "    \"minute\": 30,\n" +
                "    \"nano\": 0,\n" +
                "    \"second\": 0\n" +
                "  },\n" +
                "  \"truckWaitTimeCharge\": 150,\n" +
                "  \"truckWaitTimeChargeUnit\": \"INR\",\n" +
                "  \"type\": \"Pickup\",\n" +
                "  \"ucrReference\": \"UCR123456\"\n" +
                "}\n";
        JsonTestUtility jsonTestUtility = new JsonTestUtility();
        return jsonTestUtility.convertJsonTClazz(request, PickupDeliveryDetailsRequest.class);
    }

}
