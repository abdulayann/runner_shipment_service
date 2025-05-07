package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.DocumentRequest;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.responses.ApiError;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dto.request.ListContractRequest;
import com.dpw.runner.shipment.services.dto.request.ListContractsWithFilterRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchOffersRequestFromUI;
import com.dpw.runner.shipment.services.dto.response.FetchOffersResponse;
import com.dpw.runner.shipment.services.dto.response.ListContractResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.npm.NPMContractsResponse;
import com.dpw.runner.shipment.services.dto.response.npm.NPMContractsRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.npm.NPMFetchLangChargeCodeResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.BookingCharges;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.exception.exceptions.NPMException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.response.NpmErrorResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IQuoteContractsService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.Mockito;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.commons.constants.NPMConstants.ANY;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {NPMServiceAdapter.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
@Execution(ExecutionMode.CONCURRENT)
class NPMServiceAdapterTest {
    @MockBean
    private IAwbDao iAwbDao;

    @MockBean
    private ICustomerBookingDao iCustomerBookingDao;

    @MockBean
    private IShipmentService iShipmentService;

    @MockBean
    private IV1Service iV1Service;

    @MockBean
    private JsonHelper jsonHelper;

    @MockBean
    private ModelMapper modelMapper;

    @Autowired
    private NPMServiceAdapter nPMServiceAdapter;

    @MockBean
    private MasterDataUtils masterDataUtils;

    @MockBean
    private IQuoteContractsService quoteContractsService;

    @MockBean(name = "restTemplateForNpmService")
    private RestTemplate restTemplate;

    @MockBean(name = "restTemplateForNpmMultiLangChargeCode")
    private RestTemplate restTemplate2;

    @MockBean(name = "restTemplateForNPM")
    private RestTemplate restTemplate3;

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchContract(CommonRequestModel)}
     */
    @Test
    void testFetchContract() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act
        ResponseEntity<IRunnerResponse> actualFetchContractResult = nPMServiceAdapter.fetchContract(commonRequestModel);

        // Assert
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(isNull());
        verify(restTemplate3).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(((DependentServiceResponse) actualFetchContractResult.getBody()).getError());
        assertNull(((DependentServiceResponse) actualFetchContractResult.getBody()).getData());
        assertNull(((DependentServiceResponse) actualFetchContractResult.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualFetchContractResult.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualFetchContractResult.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualFetchContractResult.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualFetchContractResult.getStatusCode());
        assertTrue(((DependentServiceResponse) actualFetchContractResult.getBody()).isSuccess());
        assertTrue(actualFetchContractResult.hasBody());
        assertTrue(actualFetchContractResult.getHeaders().isEmpty());
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchContract(CommonRequestModel)}
     */
    @Test
    void testFetchContract2() throws RunnerException, RestClientException {
        // Arrange
        NpmErrorResponse buildResult = NpmErrorResponse.builder().errorMessage("An error occurred").success(true).build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenReturn(buildResult);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(NPMException.class, () -> nPMServiceAdapter.fetchContract(commonRequestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(Mockito.<NpmErrorResponse>any());
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
        verify(restTemplate3).exchange(isA(RequestEntity.class), isA(Class.class));
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchContract(CommonRequestModel)}
     */
    @Test
    void testFetchContract3() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(HttpClientErrorException.class, () -> nPMServiceAdapter.fetchContract(commonRequestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(isNull());
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
        verify(restTemplate3).exchange(isA(RequestEntity.class), isA(Class.class));
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchContract(CommonRequestModel)}
     */
    @Test
    void testFetchContract4() throws RunnerException {
        // Arrange
        NpmErrorResponse buildResult = NpmErrorResponse.builder().errorMessage("An error occurred").success(true).build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenReturn(buildResult);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        when(commonRequestModel.getData()).thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));

        // Act and Assert
        assertThrows(NPMException.class, () -> nPMServiceAdapter.fetchContract(commonRequestModel));
        verify(commonRequestModel).getData();
        verify(jsonHelper).convertToJson(isA(NpmErrorResponse.class));
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
    }

    @Test
    void testFetchContract5() throws RunnerException {
        ListContractResponse listContractResponse = new ListContractResponse();
        List<ListContractResponse.ContractResponse> contracts = new ArrayList<>();
        contracts.add(ListContractResponse.ContractResponse.builder().origin("1").destination("2").meta(ListContractResponse.Meta.builder().pod("1").pol("2").build()).build());
        listContractResponse.setContracts(contracts);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(listContractResponse));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(listContractResponse);
        List<UnlocationsResponse> unlocations = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setLocationsReferenceGUID("1");
        unlocations.add(unlocationsResponse);
        when(iV1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().entities(unlocations).build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(unlocationsResponse));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(new ListContractRequest()).build();
        var responseEntity = nPMServiceAdapter.fetchContract(commonRequestModel);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testFetchContract5_withCarrierANY() throws RunnerException {
        ListContractResponse listContractResponse = new ListContractResponse();
        List<ListContractResponse.ContractResponse> contracts = new ArrayList<>();
        List<String> carrierCodes = new ArrayList<>();
        carrierCodes.add(ANY);
        contracts.add(ListContractResponse.ContractResponse.builder().origin("1").destination("2").carrier_codes(carrierCodes).meta(ListContractResponse.Meta.builder().pod("1").pol("2").build()).build());
        listContractResponse.setContracts(contracts);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(listContractResponse));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(listContractResponse);
        List<UnlocationsResponse> unlocations = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setLocationsReferenceGUID("1");
        unlocations.add(unlocationsResponse);
        when(iV1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().entities(unlocations).build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(unlocationsResponse));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(new ListContractRequest()).build();
        var responseEntity = nPMServiceAdapter.fetchContract(commonRequestModel);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testFetchContract5_withCarrier() throws RunnerException {
        ListContractResponse listContractResponse = new ListContractResponse();
        List<ListContractResponse.ContractResponse> contracts = new ArrayList<>();
        List<String> carrierCodes = new ArrayList<>();
        carrierCodes.add("testCarrier");
        contracts.add(ListContractResponse.ContractResponse.builder().origin("1").destination("2").carrier_codes(carrierCodes).meta(ListContractResponse.Meta.builder().pod("1").pol("2").build()).build());
        listContractResponse.setContracts(contracts);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(listContractResponse));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(listContractResponse);
        List<UnlocationsResponse> unlocations = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setLocationsReferenceGUID("1");
        unlocations.add(unlocationsResponse);
        when(iV1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().entities(unlocations).build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(unlocationsResponse));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(new ListContractRequest()).build();
        var responseEntity = nPMServiceAdapter.fetchContract(commonRequestModel);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchContractFromShipment(CommonRequestModel)}
     */
    @Test
    void testFetchContractFromShipment() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act
        ResponseEntity<IRunnerResponse> actualFetchContractFromShipmentResult = nPMServiceAdapter
                .fetchContractFromShipment(commonRequestModel);

        // Assert
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(isNull());
        verify(restTemplate3).exchange(isA(RequestEntity.class), isA(Class.class));
        assertTrue(((DependentServiceResponse) actualFetchContractFromShipmentResult.getBody())
                .getData() instanceof ShipmentDetailsResponse);
        assertNull(((DependentServiceResponse) actualFetchContractFromShipmentResult.getBody()).getError());
        assertNull(((DependentServiceResponse) actualFetchContractFromShipmentResult.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualFetchContractFromShipmentResult.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualFetchContractFromShipmentResult.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualFetchContractFromShipmentResult.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualFetchContractFromShipmentResult.getStatusCode());
        assertTrue(((DependentServiceResponse) actualFetchContractFromShipmentResult.getBody()).isSuccess());
        assertTrue(actualFetchContractFromShipmentResult.hasBody());
        assertTrue(actualFetchContractFromShipmentResult.getHeaders().isEmpty());
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchContractFromShipment(CommonRequestModel)}
     */
    @Test
    void testFetchContractFromShipment2() throws RunnerException, RestClientException {
        // Arrange
        NpmErrorResponse buildResult = NpmErrorResponse.builder().errorMessage("An error occurred").success(true).build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenReturn(buildResult);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(NPMException.class, () -> nPMServiceAdapter.fetchContractFromShipment(commonRequestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(Mockito.<NpmErrorResponse>any());
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
        verify(restTemplate3).exchange(isA(RequestEntity.class), isA(Class.class));
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchContractFromShipment(CommonRequestModel)}
     */
    @Test
    void testFetchContractFromShipment3() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(HttpClientErrorException.class, () -> nPMServiceAdapter.fetchContractFromShipment(commonRequestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(isNull());
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
        verify(restTemplate3).exchange(isA(RequestEntity.class), isA(Class.class));
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchContractFromShipment(CommonRequestModel)}
     */
    @Test
    void testFetchContractFromShipment4() throws RunnerException {
        // Arrange
        NpmErrorResponse buildResult = NpmErrorResponse.builder().errorMessage("An error occurred").success(true).build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenReturn(buildResult);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        when(commonRequestModel.getData()).thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));

        // Act and Assert
        assertThrows(NPMException.class, () -> nPMServiceAdapter.fetchContractFromShipment(commonRequestModel));
        verify(commonRequestModel).getData();
        verify(jsonHelper).convertToJson(isA(NpmErrorResponse.class));
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
    }

    @Test
    void testFetchContractFromShipment5() throws RunnerException {
        ListContractResponse listContractResponse = new ListContractResponse();
        List<ListContractResponse.ContractResponse> contracts = new ArrayList<>();
        contracts.add(ListContractResponse.ContractResponse.builder().origin("1").destination("2").
                        load_types(List.of("LSE")).
                        meta(ListContractResponse.Meta.builder().pod("1").pol("2").mode_of_transport("AIR").
                                build()).
                build());
        listContractResponse.setContracts(contracts);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(listContractResponse));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(listContractResponse);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(new ListContractRequest()).build();
        var responseEntity = nPMServiceAdapter.fetchContractFromShipment(commonRequestModel);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testFetchContractFromShipment6() throws RunnerException {
        ListContractResponse listContractResponse = new ListContractResponse();
        List<ListContractResponse.ContractResponse> contracts = new ArrayList<>();
        contracts.add(ListContractResponse.ContractResponse.builder().origin("1").destination("2").
                        load_types(List.of("LSE")).
                        meta(ListContractResponse.Meta.builder().mode_of_transport("AIR").
                                route(List.of(ListContractResponse.Route.builder().type("LEG").build(),
                                              ListContractResponse.Route.builder().type("LEG").
                                              origin(ListContractResponse.RouteInfo.builder().build()).
                                              destination(ListContractResponse.RouteInfo.builder().build()).
                                              build()
                                     )
                                ).
                                branch_info(ListContractResponse.BranchInfo.builder().build()).
                        build()).
                        contract_usage(List.of(ListContractResponse.ContractUsage.builder().usage(1L).
                                filter_params(ListContractResponse.FilterParams.builder().cargo_type(List.of("LCL")).commodity(List.of("FAK")).build()).
                                meta(ListContractResponse.ContractUsageMeta.builder().load_attributes(ListContractResponse.LoadAttributes.builder().
                                        dimensions(ListContractResponse.Dimensions.builder().width(1L).length(2L).height(2L).uom("CM").build()).build()).build()).
                                build())).
                build());
        listContractResponse.setContracts(contracts);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(listContractResponse));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(listContractResponse);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(new ListContractRequest()).build();
        var responseEntity = nPMServiceAdapter.fetchContractFromShipment(commonRequestModel);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testFetchContractFromShipment7() throws RunnerException {
        ListContractResponse listContractResponse = new ListContractResponse();
        List<ListContractResponse.ContractResponse> contracts = new ArrayList<>();
        contracts.add(ListContractResponse.ContractResponse.builder().origin("1").destination("2").
                load_types(List.of("FCL")).
                meta(ListContractResponse.Meta.builder().mode_of_transport("SEA").
                        route(List.of(ListContractResponse.Route.builder().type("LEG").build(),
                                        ListContractResponse.Route.builder().type("LEG").
                                                origin(ListContractResponse.RouteInfo.builder().build()).
                                                destination(ListContractResponse.RouteInfo.builder().build()).
                                                build()
                                )
                        ).
                        branch_info(ListContractResponse.BranchInfo.builder().build()).
                        build()).
                contract_usage(List.of(ListContractResponse.ContractUsage.builder().usage(1L).
                        filter_params(ListContractResponse.FilterParams.builder().cargo_type(List.of("FCL")).commodity(List.of("FAK")).build()).
                        build())).
                build());
        listContractResponse.setContracts(contracts);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(listContractResponse));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(listContractResponse);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(new ListContractRequest()).build();
        var responseEntity = nPMServiceAdapter.fetchContractFromShipment(commonRequestModel);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchContracts(CommonRequestModel)}
     */
    @Test
    void testFetchContracts() throws RunnerException {
        // Arrange
        NpmErrorResponse buildResult = NpmErrorResponse.builder().errorMessage("An error occurred").success(true).build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenReturn(buildResult);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        when(commonRequestModel.getData()).thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));

        // Act and Assert
        assertThrows(NPMException.class, () -> nPMServiceAdapter.fetchContracts(commonRequestModel));
        verify(commonRequestModel).getData();
        verify(jsonHelper).convertToJson(isA(NpmErrorResponse.class));
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
    }

    @Test
    void testFetchContracts2() throws RunnerException {
        NPMContractsResponse npmContractsResponse = new NPMContractsResponse();
        List<NPMContractsResponse.NPMContractResponse> contracts = new ArrayList<>();
        contracts.add(NPMContractsResponse.NPMContractResponse.builder().origin("1").destination("2").validTill(LocalDateTime.MAX).meta(ListContractResponse.Meta.builder().pod("1").pol("2").build()).build());
        contracts.add(NPMContractsResponse.NPMContractResponse.builder().origin("1").destination("2").validTill(LocalDateTime.MIN).meta(ListContractResponse.Meta.builder().pod("1").pol("2").build()).build());
        contracts.add(NPMContractsResponse.NPMContractResponse.builder().origin("1").destination("2").meta(ListContractResponse.Meta.builder().pod("1").pol("2").build()).build());
        npmContractsResponse.setContracts(contracts);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(npmContractsResponse));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(npmContractsResponse);
        List<UnlocationsResponse> unlocations = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setLocationsReferenceGUID("1");
        unlocations.add(unlocationsResponse);
        UnlocationsResponse unlocationsResponse1 = new UnlocationsResponse();
        unlocationsResponse1.setLocationsReferenceGUID("2");
        unlocations.add(unlocationsResponse1);
        when(iV1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().entities(unlocations).build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(unlocationsResponse));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(ListContractsWithFilterRequest.builder().listContractRequest(new ListContractRequest()).build()).build();
        var responseEntity = nPMServiceAdapter.fetchContracts(commonRequestModel);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testFetchContracts3() throws RunnerException {
        NPMContractsResponse npmContractsResponse = new NPMContractsResponse();
        List<NPMContractsResponse.NPMContractResponse> contracts = new ArrayList<>();
        contracts.add(NPMContractsResponse.NPMContractResponse.builder().origin("1").destination("2").validTill(LocalDateTime.MAX).meta(ListContractResponse.Meta.builder().pod("1").pol("2").build()).build());
        contracts.add(NPMContractsResponse.NPMContractResponse.builder().origin("1").destination("2").validTill(LocalDateTime.MIN).meta(ListContractResponse.Meta.builder().pod("1").pol("2").build()).build());
        contracts.add(NPMContractsResponse.NPMContractResponse.builder().origin("1").destination("2").meta(ListContractResponse.Meta.builder().pod("1").pol("2").build()).build());
        npmContractsResponse.setContracts(contracts);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(npmContractsResponse));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(npmContractsResponse);
        List<UnlocationsResponse> unlocations = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setLocationsReferenceGUID("1");
        unlocations.add(unlocationsResponse);
        UnlocationsResponse unlocationsResponse1 = new UnlocationsResponse();
        unlocationsResponse1.setLocationsReferenceGUID("2");
        unlocations.add(unlocationsResponse1);
        when(iV1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().entities(unlocations).build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(unlocationsResponse));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(ListContractsWithFilterRequest.builder().listContractRequest(new ListContractRequest()).cargoType("LCL").origin("a").destination("b").build()).build();
        var responseEntity = nPMServiceAdapter.fetchContracts(commonRequestModel);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#updateContracts(CommonRequestModel)}
     */
    @Test
    void testUpdateContracts() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act
        ResponseEntity<IRunnerResponse> actualUpdateContractsResult = nPMServiceAdapter.updateContracts(commonRequestModel);

        // Assert
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(isNull());
        verify(restTemplate3).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(((DependentServiceResponse) actualUpdateContractsResult.getBody()).getError());
        assertNull(((DependentServiceResponse) actualUpdateContractsResult.getBody()).getData());
        assertNull(((DependentServiceResponse) actualUpdateContractsResult.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualUpdateContractsResult.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualUpdateContractsResult.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualUpdateContractsResult.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualUpdateContractsResult.getStatusCode());
        assertTrue(((DependentServiceResponse) actualUpdateContractsResult.getBody()).isSuccess());
        assertTrue(actualUpdateContractsResult.hasBody());
        assertTrue(actualUpdateContractsResult.getHeaders().isEmpty());
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#updateContracts(CommonRequestModel)}
     */
    @Test
    void testUpdateContracts2() throws RunnerException, RestClientException {
        // Arrange
        NpmErrorResponse buildResult = NpmErrorResponse.builder().errorMessage("An error occurred").success(true).build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenReturn(buildResult);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(NPMException.class, () -> nPMServiceAdapter.updateContracts(commonRequestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(Mockito.<NpmErrorResponse>any());
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
        verify(restTemplate3).exchange(isA(RequestEntity.class), isA(Class.class));
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#updateContracts(CommonRequestModel)}
     */
    @Test
    void testUpdateContracts3() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(HttpClientErrorException.class, () -> nPMServiceAdapter.updateContracts(commonRequestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(isNull());
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
        verify(restTemplate3).exchange(isA(RequestEntity.class), isA(Class.class));
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#updateContracts(CommonRequestModel)}
     */
    @Test
    void testUpdateContracts4() throws RunnerException {
        // Arrange
        NpmErrorResponse buildResult = NpmErrorResponse.builder().errorMessage("An error occurred").success(true).build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenReturn(buildResult);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        when(commonRequestModel.getData()).thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));

        // Act and Assert
        assertThrows(NPMException.class, () -> nPMServiceAdapter.updateContracts(commonRequestModel));
        verify(commonRequestModel).getData();
        verify(jsonHelper).convertToJson(isA(NpmErrorResponse.class));
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
    }

    /**
     * Method under test: {@link NPMServiceAdapter#fetchOffers(CommonRequestModel)}
     */
    @Test
    void testFetchOffers() throws RunnerException, JsonProcessingException {
        UsersDto usersDto = new UsersDto();
        usersDto.setTenantId(1);
        usersDto.setCompanyCurrency("AED");
        UserContext.setUser(usersDto);
        String json = "{\"origin\":\"INMAA_POR\",\"destination\":\"LzpprcihHCD42MNLUaM1\",\"preferred_date\":\"2024-05-22 12:40:33\",\"preferred_date_type\":\"PICKUP\",\"mode_of_transport\":\"SEA\",\"cargo_type\":\"FCL\",\"service_mode\":\"P2F\",\"fetch_default_rates\":\"false\",\"direction\":\"EXP\",\"offer_filter_type\":\"CHEAPEST\",\"weight\":null,\"volume\":null,\"volume_uom\":null,\"contracts_info\":{\"customer_org_id\":\"FRC00003392\",\"contract_id\":\"DPWQ-171434-101744\"},\"pol\":\"INMAA_POR\",\"pod\":\"USMIA_POR\",\"containers\":[{\"id\":null,\"isContractEnforced\":true,\"commodityGroup\":\"FAK\",\"containerCode\":\"20FR\",\"containerCount\":\"1\",\"contractEnforcedQuantityLimit\":100,\"customId\":\"1cbcb101-6b2f-4541-ab70-30c9a1ff8534\",\"initalCount\":100,\"masterData\":{\"commodityGroup\":\"FREIGHTALLKINDS-FAK\"},\"containerType\":\"20FR\",\"quantity\":\"1\",\"commodityCode\":\"FAK\"}]}";
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        NPMFetchOffersRequestFromUI fetchOffersRequest = objectMapper.readValue(json, NPMFetchOffersRequestFromUI.class);
        String rates = "{\"offer_type\":\"CONTRACTUAL_OFFER\",\"offers\":[{\"offer_type\":\"CONTRACTUAL_OFFER\",\"chargeable\":0.01,\"chargeable_uom\":\"M3\",\"entity_rate_cards\":[{\"loads_rates_info\":[{\"associated_rates\":[{\"slabs\":[],\"calculated_sell\":151.23,\"calculated_cost\":0,\"procured_sell\":3.24,\"procured_cost\":0,\"unit_sell\":15122.67,\"unit_cost\":0,\"procured_unit_sell\":324.23,\"procured_unit_cost\":0,\"calculated_margin\":0,\"procured_margin\":0,\"rates_uom\":\"perctr\",\"conversion_rate\":46.6417910448,\"chargeable\":0.01,\"chargeable_uom\":\"M3\",\"total_unit_count\":0.01,\"measurement_unit\":\"M3\",\"applicable_on_booking\":false,\"mode_of_transport\":\"ANY\",\"rate_classification\":\"NON-VAS\",\"rate_taxes\":[],\"charge_group\":[\"ORIGIN_CHARGES\"],\"slab_floor\":15122.67,\"slab_ceil\":15122.67,\"carrier\":\"ANY\",\"rate_type\":\"ACD\",\"rate_name\":\"AdvanceCargoDeclaration\",\"base_price_currency\":\"USD\",\"required_currency\":\"EGP\"}],\"quantity\":1}],\"aggregated_shipment_load_rates_info\":[{\"associated_rates\":[{\"slabs\":[],\"calculated_sell\":151.23,\"calculated_cost\":0,\"procured_sell\":3.24,\"procured_cost\":0,\"unit_sell\":15122.67,\"unit_cost\":0,\"procured_unit_sell\":324.23,\"procured_unit_cost\":0,\"calculated_margin\":0,\"procured_margin\":0,\"rates_uom\":\"pership\",\"conversion_rate\":46.6417910448,\"chargeable\":0.01,\"chargeable_uom\":\"M3\",\"total_unit_count\":0.01,\"measurement_unit\":\"M3\",\"applicable_on_booking\":false,\"mode_of_transport\":\"ANY\",\"rate_classification\":\"NON-VAS\",\"rate_taxes\":[],\"charge_group\":[\"ORIGIN_CHARGES\"],\"slab_floor\":15122.67,\"slab_ceil\":15122.67,\"carrier\":\"ANY\",\"rate_type\":\"ACD\",\"rate_name\":\"AdvanceCargoDeclaration\",\"base_price_currency\":\"USD\",\"required_currency\":\"EGP\"}],\"quantity\":1}]}],\"product_name\":\"LCL\",\"tenant_uuid\":\"ab2f6b79-0384-43b2-ba09-e6c38c1df6f6\",\"meta\":{\"route\":[{\"type\":\"LEG\",\"origin\":{\"code\":\"EGDAM_POR\"},\"destination\":{\"code\":\"AEJEA_POR\"}}]},\"shipment_level_rates\":[{\"slabs\":[],\"calculated_sell\":151.23,\"calculated_cost\":0,\"procured_sell\":3.24,\"procured_cost\":0,\"unit_sell\":15122.67,\"unit_cost\":0,\"procured_unit_sell\":324.23,\"procured_unit_cost\":0,\"calculated_margin\":0,\"procured_margin\":0,\"rates_uom\":\"percbm\",\"conversion_rate\":46.6417910448,\"chargeable\":0.01,\"chargeable_uom\":\"M3\",\"total_unit_count\":0.01,\"measurement_unit\":\"M3\",\"applicable_on_booking\":false,\"mode_of_transport\":\"ANY\",\"rate_classification\":\"NON-VAS\",\"rate_taxes\":[],\"charge_group\":[\"ORIGIN_CHARGES\"],\"slab_floor\":15122.67,\"slab_ceil\":15122.67,\"carrier\":\"ANY\",\"rate_type\":\"ACD\",\"rate_name\":\"AdvanceCargoDeclaration\",\"base_price_currency\":\"USD\",\"required_currency\":\"EGP\"}]}],\"reason\":\"\",\"message\":\"\"}";
        FetchOffersResponse fetchOffersResponse = objectMapper.readValue(rates, FetchOffersResponse.class);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(fetchOffersResponse));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(fetchOffersResponse);

        // Arrange and Act
        var response = nPMServiceAdapter.fetchOffers(CommonRequestModel.builder().data(fetchOffersRequest).build());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testFetchOffers_WithCarrier() throws RunnerException, JsonProcessingException {
        UsersDto usersDto = new UsersDto();
        usersDto.setTenantId(1);
        usersDto.setCompanyCurrency("AED");
        UserContext.setUser(usersDto);
        String json = "{\"origin\":\"INMAA_POR\",\"destination\":\"LzpprcihHCD42MNLUaM1\",\"preferred_date\":\"2024-05-22 12:40:33\",\"preferred_date_type\":\"PICKUP\",\"mode_of_transport\":\"SEA\",\"cargo_type\":\"FCL\",\"service_mode\":\"P2F\",\"fetch_default_rates\":\"false\",\"direction\":\"EXP\",\"offer_filter_type\":\"CHEAPEST\",\"weight\":null,\"volume\":null,\"volume_uom\":null,\"contracts_info\":{\"customer_org_id\":\"FRC00003392\",\"contract_id\":\"DPWQ-171434-101744\"},\"pol\":\"INMAA_POR\",\"pod\":\"USMIA_POR\",\"containers\":[{\"id\":null,\"isContractEnforced\":true,\"commodityGroup\":\"FAK\",\"containerCode\":\"20FR\",\"containerCount\":\"1\",\"contractEnforcedQuantityLimit\":100,\"customId\":\"1cbcb101-6b2f-4541-ab70-30c9a1ff8534\",\"initalCount\":100,\"masterData\":{\"commodityGroup\":\"FREIGHTALLKINDS-FAK\"},\"containerType\":\"20FR\",\"quantity\":\"1\",\"commodityCode\":\"FAK\"}]}";
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        NPMFetchOffersRequestFromUI fetchOffersRequest = objectMapper.readValue(json, NPMFetchOffersRequestFromUI.class);
        String rates = "{\"offer_type\":\"CONTRACTUAL_OFFER\",\"offers\":[{\"offer_type\":\"CONTRACTUAL_OFFER\",\"chargeable\":0.01,\"chargeable_uom\":\"M3\",\"entity_rate_cards\":[{\"loads_rates_info\":[{\"associated_rates\":[{\"slabs\":[],\"calculated_sell\":151.23,\"calculated_cost\":0,\"procured_sell\":3.24,\"procured_cost\":0,\"unit_sell\":15122.67,\"unit_cost\":0,\"procured_unit_sell\":324.23,\"procured_unit_cost\":0,\"calculated_margin\":0,\"procured_margin\":0,\"rates_uom\":\"perctr\",\"conversion_rate\":46.6417910448,\"chargeable\":0.01,\"chargeable_uom\":\"M3\",\"total_unit_count\":0.01,\"measurement_unit\":\"M3\",\"applicable_on_booking\":false,\"mode_of_transport\":\"ANY\",\"rate_classification\":\"NON-VAS\",\"rate_taxes\":[],\"charge_group\":[\"ORIGIN_CHARGES\"],\"slab_floor\":15122.67,\"slab_ceil\":15122.67,\"carrier\":\"ANY\",\"rate_type\":\"ACD\",\"rate_name\":\"AdvanceCargoDeclaration\",\"base_price_currency\":\"USD\",\"required_currency\":\"EGP\"}],\"quantity\":1}],\"aggregated_shipment_load_rates_info\":[{\"associated_rates\":[{\"slabs\":[],\"calculated_sell\":151.23,\"calculated_cost\":0,\"procured_sell\":3.24,\"procured_cost\":0,\"unit_sell\":15122.67,\"unit_cost\":0,\"procured_unit_sell\":324.23,\"procured_unit_cost\":0,\"calculated_margin\":0,\"procured_margin\":0,\"rates_uom\":\"pership\",\"conversion_rate\":46.6417910448,\"chargeable\":0.01,\"chargeable_uom\":\"M3\",\"total_unit_count\":0.01,\"measurement_unit\":\"M3\",\"applicable_on_booking\":false,\"mode_of_transport\":\"ANY\",\"rate_classification\":\"NON-VAS\",\"rate_taxes\":[],\"charge_group\":[\"ORIGIN_CHARGES\"],\"slab_floor\":15122.67,\"slab_ceil\":15122.67,\"carrier\":\"ANY\",\"rate_type\":\"ACD\",\"rate_name\":\"AdvanceCargoDeclaration\",\"base_price_currency\":\"USD\",\"required_currency\":\"EGP\"}],\"quantity\":1}]}],\"product_name\":\"LCL\",\"tenant_uuid\":\"ab2f6b79-0384-43b2-ba09-e6c38c1df6f6\",\"meta\":{\"route\":[{\"type\":\"LEG\",\"origin\":{\"code\":\"EGDAM_POR\"},\"destination\":{\"code\":\"AEJEA_POR\"}}]},\"shipment_level_rates\":[{\"slabs\":[],\"calculated_sell\":151.23,\"calculated_cost\":0,\"procured_sell\":3.24,\"procured_cost\":0,\"unit_sell\":15122.67,\"unit_cost\":0,\"procured_unit_sell\":324.23,\"procured_unit_cost\":0,\"calculated_margin\":0,\"procured_margin\":0,\"rates_uom\":\"percbm\",\"conversion_rate\":46.6417910448,\"chargeable\":0.01,\"chargeable_uom\":\"M3\",\"total_unit_count\":0.01,\"measurement_unit\":\"M3\",\"applicable_on_booking\":false,\"mode_of_transport\":\"ANY\",\"rate_classification\":\"NON-VAS\",\"rate_taxes\":[],\"charge_group\":[\"ORIGIN_CHARGES\"],\"slab_floor\":15122.67,\"slab_ceil\":15122.67,\"carrier\":\"ANY\",\"rate_type\":\"ACD\",\"rate_name\":\"AdvanceCargoDeclaration\",\"base_price_currency\":\"USD\",\"required_currency\":\"EGP\"}]}],\"reason\":\"\",\"message\":\"\"}";
        FetchOffersResponse fetchOffersResponse = objectMapper.readValue(rates, FetchOffersResponse.class);
        fetchOffersResponse.getOffers().get(0).setCarrier("Code");
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(fetchOffersResponse));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(fetchOffersResponse);

        // Arrange and Act
        var response = nPMServiceAdapter.fetchOffers(CommonRequestModel.builder().data(fetchOffersRequest).build());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testFetchOffers2() throws RunnerException, JsonProcessingException {
        UsersDto usersDto = new UsersDto();
        usersDto.setTenantId(1);
        usersDto.setCompanyCurrency("AED");
        UserContext.setUser(usersDto);
        String json = "{\"booking_id\":147796,\"origin\":\"INMAA_POR\",\"destination\":\"LzpprcihHCD42MNLUaM1\",\"preferred_date\":\"2024-05-22 12:44:08\",\"preferred_date_type\":\"PICKUP\",\"mode_of_transport\":\"SEA\",\"cargo_type\":\"LCL\",\"service_mode\":\"P2F\",\"fetch_default_rates\":\"false\",\"direction\":\"EXP\",\"offer_filter_type\":\"CHEAPEST\",\"contracts_info\":{\"customer_org_id\":\"FRC00003392\",\"contract_id\":\"DPWQ-171434-101744\"},\"pol\":\"INMAA_POR\",\"pod\":\"USMIA_POR\",\"containers\":[{\"id\":151467,\"guid\":\"cb9898fb-f0d9-4739-9d79-c7c99f76a6df\",\"bookingId\":147796,\"containerCode\":\"20FR\",\"containerCount\":\"2\",\"commodityGroup\":\"FAK\",\"isContractEnforced\":true,\"masterData\":{\"commodityGroup\":\"FREIGHTALLKINDS-FAK\"},\"containerCodeData\":{\"containerCode\":\"20FR-Twentyfootflatrack\"},\"contractEnforcedQuantityLimit\":100,\"isAttached\":false,\"eventsList\":[],\"tenantId\":908,\"customId\":\"b7ff5346-e776-4a03-8db9-0b4d22a9b7ae\",\"initalCount\":1,\"containerType\":\"20FR\",\"quantity\":\"2\",\"commodityCode\":\"FAK\"}]}";
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        NPMFetchOffersRequestFromUI fetchOffersRequest = objectMapper.readValue(json, NPMFetchOffersRequestFromUI.class);
        FetchOffersResponse fetchOffersResponse = new FetchOffersResponse();
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(fetchOffersResponse));
        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setBookingCharges(List.of(new BookingCharges()));
        Containers container = Containers.builder().containerCode("20GP").bookingId(147796L).build();
        customerBooking.setContainersList(List.of(container));
        customerBooking.setPackingList(new ArrayList<>());
        when(iCustomerBookingDao.findById(any())).thenReturn(Optional.of(customerBooking));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(fetchOffersResponse);

        // Arrange and Act
        var response = nPMServiceAdapter.fetchOffers(CommonRequestModel.builder().data(fetchOffersRequest).build());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testFetchOffers3() throws RunnerException, JsonProcessingException {
        UsersDto usersDto = new UsersDto();
        usersDto.setTenantId(1);
        usersDto.setCompanyCurrency("AED");
        UserContext.setUser(usersDto);
        String json = "{\"booking_id\":147796,\"origin\":\"EGDAM_POR\",\"destination\":\"AEJEA_POR\",\"preferred_date\":\"2024-05-22 13:37:06\",\"preferred_date_type\":\"PICKUP\",\"mode_of_transport\":\"SEA\",\"cargo_type\":\"LCL\",\"service_mode\":\"P2P\",\"fetch_default_rates\":\"false\",\"direction\":\"EXP\",\"offer_filter_type\":\"CHEAPEST\",\"weight\":null,\"weight_uom\":null,\"volume\":null,\"volume_uom\":null,\"contracts_info\":{\"customer_org_id\":\"FRC00003392\",\"contract_id\":\"DPWQ-865114-103001\"},\"pol\":\"EGDAM_POR\",\"pod\":\"AEJEA_POR\",\"packs\":[{\"id\":null,\"isContractEnforced\":true,\"commodityGroup\":\"FAK\",\"packsType\":\"MPK\",\"packs\":1,\"contractEnforcedQuantityLimit\":1,\"weight\":100,\"volume\":100,\"height\":null,\"width\":null,\"length\":null,\"weightUnit\":\"KG\",\"volumeUnit\":\"CC\",\"heightUnit\":null,\"widthUnit\":null,\"lengthUnit\":null,\"isDimension\":false,\"quantity\":1,\"commodity\":\"FAK\"}]}";
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        NPMFetchOffersRequestFromUI fetchOffersRequest = objectMapper.readValue(json, NPMFetchOffersRequestFromUI.class);
        FetchOffersResponse fetchOffersResponse = new FetchOffersResponse();
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(fetchOffersResponse));
        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setBookingCharges(List.of(new BookingCharges()));
        customerBooking.setContainersList(new ArrayList<>());
        Packing packing = new Packing();
        packing.setBookingId(147796L);
        packing.setPacks("1");
        packing.setPacksType("MPK");
        customerBooking.setPackingList(List.of(packing));
        when(iCustomerBookingDao.findById(any())).thenReturn(Optional.of(customerBooking));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(fetchOffersResponse);

        // Arrange and Act
        var response = nPMServiceAdapter.fetchOffers(CommonRequestModel.builder().data(fetchOffersRequest).build());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testFetchOffers4() {
        UsersDto usersDto = new UsersDto();
        usersDto.setTenantId(1);
        usersDto.setCompanyCurrency("AED");
        UserContext.setUser(usersDto);
        NpmErrorResponse buildResult = NpmErrorResponse.builder().errorMessage("An error occurred").success(true).build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenReturn(buildResult);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(mock);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(new NPMFetchOffersRequestFromUI()).build();
        // Act and Assert
        assertThrows(NPMException.class, () -> nPMServiceAdapter.fetchOffers(commonRequestModel));
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchOffersV8(CommonRequestModel)}
     */
    @Test
    void testFetchOffersV8() throws RunnerException, JsonProcessingException {
        UsersDto usersDto = new UsersDto();
        usersDto.setTenantId(1);
        usersDto.setCompanyCurrency("AED");
        UserContext.setUser(usersDto);
        String json = "{\"origin\":\"INMAA_POR\",\"destination\":\"LzpprcihHCD42MNLUaM1\",\"preferred_date\":\"2024-05-22 12:40:33\",\"preferred_date_type\":\"PICKUP\",\"mode_of_transport\":\"SEA\",\"cargo_type\":\"FCL\",\"service_mode\":\"P2F\",\"fetch_default_rates\":\"false\",\"direction\":\"EXP\",\"offer_filter_type\":\"CHEAPEST\",\"weight\":null,\"volume\":null,\"volume_uom\":null,\"contracts_info\":{\"customer_org_id\":\"FRC00003392\",\"contract_id\":\"DPWQ-171434-101744\"},\"pol\":\"INMAA_POR\",\"pod\":\"USMIA_POR\",\"containers\":[{\"id\":null,\"isContractEnforced\":true,\"commodityGroup\":\"FAK\",\"containerCode\":\"20FR\",\"containerCount\":\"1\",\"contractEnforcedQuantityLimit\":100,\"customId\":\"1cbcb101-6b2f-4541-ab70-30c9a1ff8534\",\"initalCount\":100,\"masterData\":{\"commodityGroup\":\"FREIGHTALLKINDS-FAK\"},\"containerType\":\"20FR\",\"quantity\":\"1\",\"commodityCode\":\"FAK\"}]}";
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        NPMFetchOffersRequestFromUI fetchOffersRequest = objectMapper.readValue(json, NPMFetchOffersRequestFromUI.class);
        FetchOffersResponse fetchOffersResponse = new FetchOffersResponse();
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(fetchOffersResponse));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(fetchOffersResponse);

        // Arrange and Act
        var response = nPMServiceAdapter.fetchOffersV8(CommonRequestModel.builder().data(fetchOffersRequest).build());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testFetchOffersV8_2() throws RunnerException, JsonProcessingException {
        UsersDto usersDto = new UsersDto();
        usersDto.setTenantId(1);
        usersDto.setCompanyCurrency("AED");
        UserContext.setUser(usersDto);
        String json = "{\"booking_id\":147796,\"origin\":\"INMAA_POR\",\"destination\":\"LzpprcihHCD42MNLUaM1\",\"preferred_date\":\"2024-05-22 12:44:08\",\"preferred_date_type\":\"PICKUP\",\"mode_of_transport\":\"SEA\",\"cargo_type\":\"LCL\",\"service_mode\":\"P2F\",\"fetch_default_rates\":\"false\",\"direction\":\"EXP\",\"offer_filter_type\":\"CHEAPEST\",\"contracts_info\":{\"customer_org_id\":\"FRC00003392\",\"contract_id\":\"DPWQ-171434-101744\"},\"pol\":\"INMAA_POR\",\"pod\":\"USMIA_POR\",\"containers\":[{\"id\":151467,\"guid\":\"cb9898fb-f0d9-4739-9d79-c7c99f76a6df\",\"bookingId\":147796,\"containerCode\":\"20FR\",\"containerCount\":\"2\",\"commodityGroup\":\"FAK\",\"isContractEnforced\":true,\"masterData\":{\"commodityGroup\":\"FREIGHTALLKINDS-FAK\"},\"containerCodeData\":{\"containerCode\":\"20FR-Twentyfootflatrack\"},\"contractEnforcedQuantityLimit\":100,\"isAttached\":false,\"eventsList\":[],\"tenantId\":908,\"customId\":\"b7ff5346-e776-4a03-8db9-0b4d22a9b7ae\",\"initalCount\":1,\"containerType\":\"20FR\",\"quantity\":\"2\",\"commodityCode\":\"FAK\"}]}";
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        NPMFetchOffersRequestFromUI fetchOffersRequest = objectMapper.readValue(json, NPMFetchOffersRequestFromUI.class);
        FetchOffersResponse fetchOffersResponse = new FetchOffersResponse();
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(fetchOffersResponse));
        CustomerBooking customerBooking = new CustomerBooking();
        BookingCharges bookingCharges = new BookingCharges();
        bookingCharges.setCostAccount("1");
        customerBooking.setBookingCharges(List.of(bookingCharges));
        Containers container = Containers.builder().containerCode("20GP").bookingId(147796L).build();
        customerBooking.setContainersList(List.of(container));
        customerBooking.setPackingList(new ArrayList<>());
        when(iCustomerBookingDao.findById(any())).thenReturn(Optional.of(customerBooking));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(fetchOffersResponse);

        // Arrange and Act
        var response = nPMServiceAdapter.fetchOffersV8(CommonRequestModel.builder().data(fetchOffersRequest).build());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testFetchOffersV8_3() throws RunnerException, JsonProcessingException {
        UsersDto usersDto = new UsersDto();
        usersDto.setTenantId(1);
        usersDto.setCompanyCurrency("AED");
        UserContext.setUser(usersDto);
        String json = "{\"booking_id\":147796,\"origin\":\"EGDAM_POR\",\"destination\":\"AEJEA_POR\",\"preferred_date\":\"2024-05-22 13:37:06\",\"preferred_date_type\":\"PICKUP\",\"mode_of_transport\":\"SEA\",\"cargo_type\":\"LCL\",\"service_mode\":\"P2P\",\"fetch_default_rates\":\"false\",\"direction\":\"EXP\",\"offer_filter_type\":\"CHEAPEST\",\"weight\":null,\"weight_uom\":null,\"volume\":null,\"volume_uom\":null,\"contracts_info\":{\"customer_org_id\":\"FRC00003392\",\"contract_id\":\"DPWQ-865114-103001\"},\"pol\":\"EGDAM_POR\",\"pod\":\"AEJEA_POR\",\"packs\":[{\"id\":null,\"isContractEnforced\":true,\"commodityGroup\":\"FAK\",\"packsType\":\"MPK\",\"packs\":1,\"contractEnforcedQuantityLimit\":1,\"weight\":100,\"volume\":100,\"height\":null,\"width\":null,\"length\":null,\"weightUnit\":\"KG\",\"volumeUnit\":\"CC\",\"heightUnit\":null,\"widthUnit\":null,\"lengthUnit\":null,\"isDimension\":false,\"quantity\":1,\"commodity\":\"FAK\"}]}";
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        NPMFetchOffersRequestFromUI fetchOffersRequest = objectMapper.readValue(json, NPMFetchOffersRequestFromUI.class);
        FetchOffersResponse fetchOffersResponse = new FetchOffersResponse();
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(ResponseEntity.ok(fetchOffersResponse));
        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setBookingCharges(List.of(new BookingCharges()));
        customerBooking.setContainersList(new ArrayList<>());
        Packing packing = new Packing();
        packing.setBookingId(147796L);
        packing.setPacks("1");
        packing.setPacksType("MPK");
        customerBooking.setPackingList(List.of(packing));
        when(iCustomerBookingDao.findById(any())).thenReturn(Optional.of(customerBooking));
        var mock = mock(ResponseEntity.class);
        when(mock.getBody()).thenReturn(fetchOffersResponse);

        // Arrange and Act
        var response = nPMServiceAdapter.fetchOffersV8(CommonRequestModel.builder().data(fetchOffersRequest).build());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testFetchOffersV8_4() {
        NpmErrorResponse buildResult = NpmErrorResponse.builder().errorMessage("An error occurred").success(true).build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenReturn(buildResult);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        when(commonRequestModel.getData()).thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));

        // Act and Assert
        assertThrows(NPMException.class, () -> nPMServiceAdapter.fetchOffersV8(commonRequestModel));
    }

    /**
     * Method under test: {@link NPMServiceAdapter#awbAutoSell(CommonRequestModel)}
     */
    @Test
    void testAwbAutoSell() throws RunnerException, RestClientException {
        // Arrange
        RunnerResponse.RunnerResponseBuilder<Object> builderResult = RunnerResponse.builder();
        RunnerResponse.RunnerResponseBuilder<Object> dataResult = builderResult.count(3L).data("Data");
        RunnerResponse<Object> buildResult = dataResult.error(new ApiError(HttpStatus.CONTINUE))
                .pageNo(1)
                .requestId("42")
                .success(true)
                .build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<RunnerResponse<Object>>>any()))
                .thenReturn(buildResult);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult2 = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult2.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(NPMException.class, () -> nPMServiceAdapter.awbAutoSell(commonRequestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(Mockito.<ApiError>any());
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
    }

    /**
     * Method under test: {@link NPMServiceAdapter#awbAutoSell(CommonRequestModel)}
     */
    @Test
    void testAwbAutoSell2() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<RunnerResponse<Object>>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(HttpClientErrorException.class, () -> nPMServiceAdapter.awbAutoSell(commonRequestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper).convertToJson(isNull());
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#awbImportRates(CommonRequestModel)}
     */
    @Test
    void testAwbImportRates() throws RunnerException, RestClientException {
        // Arrange
        RunnerResponse.RunnerResponseBuilder<Object> builderResult = RunnerResponse.builder();
        RunnerResponse.RunnerResponseBuilder<Object> dataResult = builderResult.count(3L).data("Data");
        RunnerResponse<Object> buildResult = dataResult.error(new ApiError(HttpStatus.CONTINUE))
                .pageNo(1)
                .requestId("42")
                .success(true)
                .build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<RunnerResponse<Object>>>any()))
                .thenReturn(buildResult);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult2 = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult2.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(NPMException.class, () -> nPMServiceAdapter.awbImportRates(commonRequestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(Mockito.<ApiError>any());
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#awbImportRates(CommonRequestModel)}
     */
    @Test
    void testAwbImportRates2() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<RunnerResponse<Object>>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(HttpClientErrorException.class, () -> nPMServiceAdapter.awbImportRates(commonRequestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper).convertToJson(isNull());
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchMultiLangChargeCode(CommonRequestModel)}
     */
    @Test
    void testFetchMultiLangChargeCode() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act
        NPMFetchLangChargeCodeResponse actualFetchMultiLangChargeCodeResult = nPMServiceAdapter
                .fetchMultiLangChargeCode(commonRequestModel);

        // Assert
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(isNull());
        verify(restTemplate3).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(actualFetchMultiLangChargeCodeResult);
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchMultiLangChargeCode(CommonRequestModel)}
     */
    @Test
    void testFetchMultiLangChargeCode2() throws RunnerException, RestClientException {
        // Arrange
        NpmErrorResponse buildResult = NpmErrorResponse.builder().errorMessage("An error occurred").success(true).build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenReturn(buildResult);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(NPMException.class, () -> nPMServiceAdapter.fetchMultiLangChargeCode(commonRequestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(Mockito.<NpmErrorResponse>any());
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
        verify(restTemplate3).exchange(isA(RequestEntity.class), isA(Class.class));
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchMultiLangChargeCode(CommonRequestModel)}
     */
    @Test
    void testFetchMultiLangChargeCode3() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel commonRequestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(HttpClientErrorException.class, () -> nPMServiceAdapter.fetchMultiLangChargeCode(commonRequestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(isNull());
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
        verify(restTemplate3).exchange(isA(RequestEntity.class), isA(Class.class));
    }

    /**
     * Method under test:
     * {@link NPMServiceAdapter#fetchMultiLangChargeCode(CommonRequestModel)}
     */
    @Test
    void testFetchMultiLangChargeCode4() throws RunnerException {
        // Arrange
        NpmErrorResponse buildResult = NpmErrorResponse.builder().errorMessage("An error occurred").success(true).build();
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<NpmErrorResponse>>any()))
                .thenReturn(buildResult);
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        when(commonRequestModel.getData()).thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));

        // Act and Assert
        assertThrows(NPMException.class, () -> nPMServiceAdapter.fetchMultiLangChargeCode(commonRequestModel));
        verify(commonRequestModel).getData();
        verify(jsonHelper).convertToJson(isA(NpmErrorResponse.class));
        verify(jsonHelper).readFromJson(eq(""), isA(Class.class));
    }

    @Test
    void getCarrier() {
        ListContractResponse.ContractResponse contractResponse = ListContractResponse.ContractResponse.builder().build();
        String response = nPMServiceAdapter.getCarrier(contractResponse);
        assertNull(response);
    }

    @Test
    void getCarrier_Empty() {
        ListContractResponse.ContractResponse contractResponse = ListContractResponse.ContractResponse.builder().carrier_codes(new ArrayList<>()).build();
        String response = nPMServiceAdapter.getCarrier(contractResponse);
        assertNull(response);
    }

    @Test
    void getCarrier_ANY() {
        List<String> carriers = new ArrayList<>();
        carriers.add(ANY);
        ListContractResponse.ContractResponse contractResponse = ListContractResponse.ContractResponse.builder().carrier_codes(carriers).build();
        String response = nPMServiceAdapter.getCarrier(contractResponse);
        assertNull(response);
    }

    @Test
    void getCarrier_test() {
        List<String> carriers = new ArrayList<>();
        carriers.add("test");
        ListContractResponse.ContractResponse contractResponse = ListContractResponse.ContractResponse.builder().carrier_codes(carriers).build();
        String response = nPMServiceAdapter.getCarrier(contractResponse);
        assertNull(response);
    }

    @Test
    void testFetchContractsWithDgEnabled() throws RunnerException {
        when(jsonHelper.convertToJson(Mockito.<Object>any())).thenReturn("Convert To Json");
        NPMContractsResponse.NPMContractResponse dgContract = new NPMContractsResponse.NPMContractResponse();
        dgContract.setProduct_type("CargoType1");
        dgContract.setOrigin("Origin1");
        dgContract.setDestination("Destination1");
        dgContract.setValidTill(LocalDateTime.now().plusDays(1));
        dgContract.setDgClass(List.of("3"));
        NPMContractsResponse.NPMContractResponse nonDgContract = new NPMContractsResponse.NPMContractResponse();
        nonDgContract.setProduct_type("CargoType2");
        nonDgContract.setOrigin("Origin2");
        nonDgContract.setDestination("Destination2");
        nonDgContract.setValidTill(LocalDateTime.now().plusDays(1));
        nonDgContract.setDgClass(List.of("NULL"));

        NPMContractsResponse npmContractsResponse = new NPMContractsResponse();
        npmContractsResponse.setContracts(Arrays.asList(dgContract, nonDgContract));

        when(restTemplate3.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity(npmContractsResponse, HttpStatus.OK));

        List<UnlocationsResponse> unlocations = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setLocationsReferenceGUID("1");
        unlocations.add(unlocationsResponse);
        when(iV1Service.fetchUnlocation(any())).thenReturn(V1DataResponse.builder().entities(unlocations).build());
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(List.of(unlocationsResponse));

        ListContractsWithFilterRequest listContractsWithFilterRequest = new ListContractsWithFilterRequest();
        listContractsWithFilterRequest.setIsDgEnabled(true);
        listContractsWithFilterRequest.setCargoType("CargoType1");
        listContractsWithFilterRequest.setOrigin("Origin1");
        listContractsWithFilterRequest.setDestination("Destination1");

        CommonRequestModel commonRequestModel = CommonRequestModel.builder()
                .data(listContractsWithFilterRequest)
                .guid("1234")
                .id(1L)
                .dependentData("Dependent Data")
                .build();

        ResponseEntity<IRunnerResponse> result = nPMServiceAdapter.fetchContracts(commonRequestModel);

        // Verify
        verify(restTemplate3).exchange(isA(RequestEntity.class), isA(Class.class));
        verify(jsonHelper, atLeastOnce()).convertToJson(any());

        DependentServiceResponse response = (DependentServiceResponse) result.getBody();
        assertNotNull(response);
        assertTrue(response.isSuccess());
        assertEquals(HttpStatus.OK, result.getStatusCode());

        List<NPMContractsRunnerResponse> responseList = (List<NPMContractsRunnerResponse>) response.getData();
        assertNotNull(responseList);
        assertFalse(responseList.isEmpty());

        List<NPMContractsResponse.NPMContractResponse> contracts = responseList.get(0).getContracts();
        assertEquals(1, contracts.size());
        assertEquals("CargoType1", contracts.get(0).getProduct_type());
        assertEquals("Origin1", contracts.get(0).getOrigin());
        assertEquals("Destination1", contracts.get(0).getDestination());
        assertEquals(List.of("3"), contracts.get(0).getDgClass());
    }
}
