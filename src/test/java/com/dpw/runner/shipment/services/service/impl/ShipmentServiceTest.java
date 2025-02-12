package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.config.SpringContext;
import com.dpw.runner.shipment.services.dao.impl.NetworkTransferDao;
import com.dpw.runner.shipment.services.dao.impl.QuartzJobInfoDao;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.mapper.AttachListShipmentMapper;
import com.dpw.runner.shipment.services.dto.mapper.ShipmentMapper;
import com.dpw.runner.shipment.services.dto.patchrequest.CarrierPatchRequest;
import com.dpw.runner.shipment.services.dto.patchrequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.awb.AwbGoodsDescriptionInfo;
import com.dpw.runner.shipment.services.dto.request.billing.InvoicePostingValidationRequest;
import com.dpw.runner.shipment.services.dto.request.notification.PendingNotificationRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGApprovalRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.response.billing.InvoicePostingValidationResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingShipmentActionsResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.ContainerBase;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Container;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.DateAndSources;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Event;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Journey;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceLiteContainerResponse;
import com.dpw.runner.shipment.services.dto.v1.request.PartiesOrgAddressRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TIContainerListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TIListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TaskCreateRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.MasterDataHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.mapper.CarrierDetailsMapper;
import com.dpw.runner.shipment.services.mapper.ShipmentDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IQuartzJobInfoRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.AuditLogsSyncRequest;
import com.dpw.runner.shipment.services.syncing.Entity.AuditLogRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import com.dpw.runner.shipment.services.syncing.impl.SyncEntityConversionService;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.http.auth.AuthenticationException;
import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.openapitools.jackson.nullable.JsonNullable;
import org.springframework.context.ApplicationContext;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyList;
import static org.mockito.Mockito.anyLong;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class
ShipmentServiceTest extends CommonMocks {

    @InjectMocks
    private ShipmentService shipmentService;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IHblDao hblDao;
    @Mock
    private IShipmentRepository shipmentRepository;
    @Mock
    private IShipmentSync shipmentSync;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private INotificationService notificationService;
    @Mock
    private CarrierDetailsMapper carrierDetailsMapper;
    @Mock
    private IV1Service v1Service;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private IHblService hblService;
    @Mock
    private KafkaProducer kafkaProducer;
    @Mock
    private ITrackingServiceAdapter trackingServiceAdapter;
    @Mock
    private ProductIdentifierUtility productEngine;
    @Mock
    private IContainerDao containerDao;
    @Mock
    private IBookingCarriageDao bookingCarriageDao;
    @Mock
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    @Mock
    private IPackingDao packingDao;
    @Mock
    private PartialFetchUtils partialFetchUtils;
    @Mock
    private IELDetailsDao elDetailsDao;
    @Mock
    private IEventDao eventDao;
    @Mock
    private INotesDao notesDao;
    @Mock
    private IAwbDao awbDao;
    @Mock
    private DependentServiceHelper dependentServiceHelper;
    @Mock
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;
    @Mock
    private IReferenceNumbersDao referenceNumbersDao;
    @Mock
    private IRoutingsDao routingsDao;
    @Mock
    private IServiceDetailsDao serviceDetailsDao;
    @Mock
    private IPartiesDao partiesDao;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private IAdditionalDetailDao additionalDetailDao;
    @Mock
    private ShipmentDetailsMapper shipmentDetailsMapper;
    @Mock
    private IShipmentOrderDao shipmentOrderDao;
    @Mock
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @Mock
    private IEventService eventService;
    @Mock
    private IAuditLogService auditLogService;
    @Mock
    private SyncEntityConversionService syncEntityConversionService;
    @Mock
    private IContainerService containerService;
    @Mock
    private IPackingService packingService;
    @Mock
    private IAwbService awbService;
    @Mock
    private IOrderManagementAdapter orderManagementAdapter;
    @Mock
    private V1ServiceUtil v1ServiceUtil;

    @Mock
    private ObjectMapper mockObjectMapper;
    @Mock
    ConsolidationService consolidationService;
    @Mock
    private HttpServletResponse httpServletResponse;
    @Mock
    private GetNextNumberHelper getNextNumberHelper;
    @Mock
    private MasterDataHelper masterDataHelper;
    @Mock
    private IRoutingsService routingsService;
    @Mock
    private DateTimeChangeLogService dateTimeChangeLogService;

    @Mock
    private RestTemplate restTemplate;

    @Mock
    private BillingServiceAdapter billingServiceAdapter;

    @Mock
    private ICarrierDetailsDao carrierDetailsDao;

    @Mock
    private INetworkTransferService networkTransferService;

    @Captor
    private ArgumentCaptor<Workbook> workbookCaptor;

    private ExecutorService executorService;

    @Mock
    private ApplicationContext applicationContext;

    @Mock
    private ConsolidationDetails consolidationDetails;

    @Mock
    private IMDMServiceAdapter mdmServiceAdapter;

    @Mock
    private IDpsEventService dpsEventService;

    @Mock
    private QuartzJobInfoService quartzJobInfoService;

    @Mock
    private QuartzJobInfoDao quartzJobInfoDao;

    @Mock
    private NetworkTransferDao networkTransferDao;

    @Mock
    private IQuartzJobInfoRepository quartzJobInfoRepository;

    @Mock
    private ICommonErrorLogsDao commonErrorLogsDao;

    @Mock
    private INotificationDao notificationDao;


    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static ShipmentDetails shipmentDetails;
    private static ConsolidationDetails testConsol;
    private UpdateConsoleShipmentRequest updateConsoleShipmentRequest;
    private InvoicePostingValidationRequest request;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
    }

    @AfterEach
    void tearDown() {
        shipmentService.executorService.shutdown();
    }

    @BeforeEach
    void setup() {
        shipmentDetails = jsonTestUtility.getTestShipment();
        request = new InvoicePostingValidationRequest();
        testConsol = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetails.class);
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        shipmentService.executorService = Executors.newFixedThreadPool(2);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(false).build());
        updateConsoleShipmentRequest = new UpdateConsoleShipmentRequest();
    }

    @Test
    void testHblCheck_HblNumberFoundInDifferentTenant() {
        String hblNumber = "HBL123";
        String shipmentId = "sampleShipmentId";
        ShipmentDetailsProjection projection1 = new ShipmentDetailsProjection() {
            @Override
            public Integer getTenantId() { return 1; }

            @Override
            public String getHblNumber() {
                return "HBL123";
            }

            @Override
            public String getShipmentId() {
                return "SHIPID1";
            }

            @Override
            public String getShipmentType() {
                return null;
            }

            @Override
            public String getTransportMode() {
                return null;
            }

            @Override
            public Long getId() {
                return null;
            }

        };

        ShipmentDetailsProjection projection2 = new ShipmentDetailsProjection() {
            @Override
            public Integer getTenantId() { return 2; }

            @Override
            public String getHblNumber() {
                return "HBL345";
            }

            @Override
            public String getShipmentId() {
                return "SHIPID2";
            }

            @Override
            public String getShipmentType() {
                return null;
            }

            @Override
            public String getTransportMode() {
                return null;
            }

            @Override
            public Long getId() {
                return null;
            }

        };

        List<ShipmentDetailsProjection> projections = Arrays.asList(projection1, projection2);

        when(shipmentDao.findByHblNumberAndExcludeShipmentId(hblNumber, shipmentId)).thenReturn(projections);

        ResponseEntity<IRunnerResponse> response = shipmentService.hblCheck(hblNumber, shipmentId);

        assertNotNull(response);
        assertNotNull(response.getBody());
        assertTrue(response.getBody() instanceof RunnerResponse);

        RunnerResponse runnerResponse = (RunnerResponse) response.getBody();

        IRunnerResponse data = (IRunnerResponse) runnerResponse.getData();
        assertTrue(data instanceof HblCheckResponse);

        HblCheckResponse hblCheckResponse = (HblCheckResponse) data;
        assertNotNull(hblCheckResponse.getMessage());

        verify(shipmentDao).findByHblNumberAndExcludeShipmentId(hblNumber, shipmentId);
    }

    @Test
    void testHblCheck_HblNumberNotFoundInDifferentTenant() {
        String hblNumber = "HBL123";
        String shipmentId = "sampleShipmentId";
        List<ShipmentDetailsProjection> projections = Collections.emptyList();

        when(shipmentDao.findByHblNumberAndExcludeShipmentId(hblNumber, shipmentId)).thenReturn(projections);

        ResponseEntity<IRunnerResponse> response = shipmentService.hblCheck(hblNumber, shipmentId);

        assertNotNull(response);
        assertNotNull(response.getBody());
        assertTrue(response.getBody() instanceof RunnerResponse);

        RunnerResponse runnerResponse = (RunnerResponse) response.getBody();

        IRunnerResponse data = (IRunnerResponse) runnerResponse.getData();
        assertTrue(data instanceof HblCheckResponse);

        HblCheckResponse hblCheckResponse = (HblCheckResponse) data;
        assertNull(hblCheckResponse.getMessage());

        verify(shipmentDao).findByHblNumberAndExcludeShipmentId(hblNumber, shipmentId);
    }

    @Test
    void testHblCheck_HblNumberIsNull() {
        String hblNumber = null;
        String shipmentId = "sampleShipmentId";
        List<ShipmentDetailsProjection> projections = Collections.emptyList();

        when(shipmentDao.findByHblNumberAndExcludeShipmentId(hblNumber, shipmentId)).thenReturn(projections);

        ResponseEntity<IRunnerResponse> response = shipmentService.hblCheck(hblNumber, shipmentId);

        assertNotNull(response);
        assertNotNull(response.getBody());
        assertTrue(response.getBody() instanceof RunnerResponse);

        RunnerResponse runnerResponse = (RunnerResponse) response.getBody();

        IRunnerResponse data = (IRunnerResponse) runnerResponse.getData();
        assertTrue(data instanceof HblCheckResponse);

        HblCheckResponse hblCheckResponse = (HblCheckResponse) data;
        assertNull(hblCheckResponse.getMessage());

        verify(shipmentDao).findByHblNumberAndExcludeShipmentId(hblNumber, shipmentId);
    }

    @Test
    public void testContainersWithoutContainerNumber_ShouldThrowException() {
        Containers containerWithoutNumber = new Containers();
        containerWithoutNumber.setContainerNumber(null);

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setContainersList(Set.of(containerWithoutNumber));

        assertThrows(ValidationException.class, () ->
                shipmentService.validateHblContainerNumberCondition(shipmentDetails1),
            "Please assign container number to all the containers before generating the HBL."
        );
    }

    @Test
    public void testPackingListWithoutContainerId_ShouldThrowException() {
        Packing packingWithoutContainerId = new Packing();
        packingWithoutContainerId.setContainerId(null);

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setPackingList(List.of(packingWithoutContainerId));

        assertThrows(ValidationException.class, () ->
                shipmentService.validateHblContainerNumberCondition(shipmentDetails1),
            "Container Number is Mandatory for HBL Generation, please assign the container number for all the packages in the shipment."
        );
    }

    @Test
    void testHblCheck_HblNumberFoundInDifferentTenantWithSingleEntry() {
        String hblNumber = "HBL123";
        String shipmentId = "sampleShipmentId";
        ShipmentDetailsProjection projection = new ShipmentDetailsProjection() {
            @Override
            public Integer getTenantId() { return 2; }

            @Override
            public String getHblNumber() {
                return "HBL345";
            }

            @Override
            public String getShipmentId() {
                return "SHIPID2";
            }

            @Override
            public String getShipmentType() {
                return null;
            }

            @Override
            public String getTransportMode() {
                return null;
            }

            @Override
            public Long getId() {
                return null;
            }
        };

        List<ShipmentDetailsProjection> projections = Collections.singletonList(projection);

        when(shipmentDao.findByHblNumberAndExcludeShipmentId(hblNumber, shipmentId)).thenReturn(projections);

        ResponseEntity<IRunnerResponse> response = shipmentService.hblCheck(hblNumber, shipmentId);

        assertNotNull(response);
        assertNotNull(response.getBody());
        assertTrue(response.getBody() instanceof RunnerResponse);

        RunnerResponse runnerResponse = (RunnerResponse) response.getBody();

        IRunnerResponse data = (IRunnerResponse) runnerResponse.getData();
        assertTrue(data instanceof HblCheckResponse);

        HblCheckResponse hblCheckResponse = (HblCheckResponse) data;
        assertNotNull(hblCheckResponse.getMessage());

        verify(shipmentDao).findByHblNumberAndExcludeShipmentId(hblNumber, shipmentId);
    }

    @Test
    void testValidateInvoicePosting_ValidSeaShipmentWithDrtJobType() {
        UUID guid = UUID.randomUUID();
        request.setShipmentGuids(Collections.singletonList(guid.toString()));

        shipmentDetails.setGuid(guid);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);

        when(shipmentDao.findShipmentsByGuids(anySet())).thenReturn(Collections.singletonList(shipmentDetails));

        ResponseEntity<IRunnerResponse> response = shipmentService.validateInvoicePosting(request);

        IRunnerResponse runnerResponse = response.getBody();
        assertNotNull(runnerResponse);
        assertTrue(runnerResponse instanceof RunnerResponse);

        List<InvoicePostingValidationResponse> responses = (List<InvoicePostingValidationResponse>) ((RunnerResponse) runnerResponse).getData();
        assertNotNull(responses);
        assertEquals(1, responses.size());

        InvoicePostingValidationResponse validationResponse = responses.get(0);
        assertEquals(guid.toString(), validationResponse.getShipmentGuid());
        assertTrue(validationResponse.getMissingFields().contains(ModuleValidationFieldType.CARRIER_ETA));
        assertTrue(validationResponse.getMissingFields().contains(ModuleValidationFieldType.CONTAINER_DETAILS));
        assertTrue(validationResponse.getMissingFields().contains(ModuleValidationFieldType.MBL_DETAILS));
    }

    @Test
    void testValidateInvoicePosting_ValidSeaShipmentWithDrtJobType2() {
        UUID guid = UUID.randomUUID();
        request.setShipmentGuids(Collections.singletonList(guid.toString()));
        Containers container = new Containers();
        container.setContainerNumber("123");

        shipmentDetails.setGuid(guid);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setCarrierDetails(null);
        shipmentDetails.setContainersList(Set.of(container));

        when(shipmentDao.findShipmentsByGuids(anySet())).thenReturn(Collections.singletonList(shipmentDetails));

        ResponseEntity<IRunnerResponse> response = shipmentService.validateInvoicePosting(request);

        IRunnerResponse runnerResponse = response.getBody();
        assertNotNull(runnerResponse);
        assertTrue(runnerResponse instanceof RunnerResponse);

        List<InvoicePostingValidationResponse> responses = (List<InvoicePostingValidationResponse>) ((RunnerResponse) runnerResponse).getData();
        assertNotNull(responses);
        assertEquals(1, responses.size());

        InvoicePostingValidationResponse validationResponse = responses.get(0);
        assertEquals(guid.toString(), validationResponse.getShipmentGuid());
        assertTrue(validationResponse.getMissingFields().contains(ModuleValidationFieldType.CARRIER));
        assertTrue(validationResponse.getMissingFields().contains(ModuleValidationFieldType.MBL_DETAILS));
    }

    @Test
    void testValidateInvoicePosting_ValidSeaShipmentWithNonDrtJobType() {
        UUID guid = UUID.randomUUID();
        request.setShipmentGuids(Collections.singletonList(guid.toString()));

        shipmentDetails.setGuid(guid);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_STD);

        when(shipmentDao.findShipmentsByGuids(anySet())).thenReturn(Collections.singletonList(shipmentDetails));

        ResponseEntity<IRunnerResponse> response = shipmentService.validateInvoicePosting(request);

        IRunnerResponse runnerResponse = response.getBody();
        assertNotNull(runnerResponse);
        assertTrue(runnerResponse instanceof RunnerResponse);

        List<InvoicePostingValidationResponse> responses = (List<InvoicePostingValidationResponse>) ((RunnerResponse) runnerResponse).getData();
        assertNotNull(responses);
        assertEquals(1, responses.size());

        InvoicePostingValidationResponse validationResponse = responses.get(0);
        assertEquals(guid.toString(), validationResponse.getShipmentGuid());
        assertFalse(validationResponse.getMissingFields().contains(ModuleValidationFieldType.CARRIER));
        assertFalse(validationResponse.getMissingFields().contains(ModuleValidationFieldType.CONTAINER_DETAILS));
        assertTrue(validationResponse.getMissingFields().contains(ModuleValidationFieldType.MBL_DETAILS));
    }

    @Test
    void testValidateInvoicePosting_ValidAirShipment() {
        UUID guid = UUID.randomUUID();
        request.setShipmentGuids(Collections.singletonList(guid.toString()));

        shipmentDetails.setGuid(guid);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LSE);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_STD);

        when(shipmentDao.findShipmentsByGuids(anySet())).thenReturn(Collections.singletonList(shipmentDetails));

        ResponseEntity<IRunnerResponse> response = shipmentService.validateInvoicePosting(request);

        IRunnerResponse runnerResponse = response.getBody();
        assertNotNull(runnerResponse);
        assertTrue(runnerResponse instanceof RunnerResponse);

        List<InvoicePostingValidationResponse> responses = (List<InvoicePostingValidationResponse>) ((RunnerResponse) runnerResponse).getData();
        assertNotNull(responses);
        assertEquals(1, responses.size());

        InvoicePostingValidationResponse validationResponse = responses.get(0);
        assertEquals(guid.toString(), validationResponse.getShipmentGuid());
        assertTrue(validationResponse.getMissingFields().contains(ModuleValidationFieldType.CARRIER_ETA));
        assertTrue(validationResponse.getMissingFields().contains(ModuleValidationFieldType.CARRIER_ETD));
        assertFalse(validationResponse.getMissingFields().contains(ModuleValidationFieldType.CONTAINER_DETAILS));
        assertTrue(validationResponse.getMissingFields().contains(ModuleValidationFieldType.MAWB_DETAILS));
    }

    @Test
    void testValidateInvoicePosting_InvalidShipmentGuid() {
        request.setShipmentGuids(Collections.singletonList(UUID.randomUUID().toString()));

        when(shipmentDao.findShipmentsByGuids(anySet())).thenReturn(Collections.emptyList());

        ResponseEntity<IRunnerResponse> response = shipmentService.validateInvoicePosting(request);

        IRunnerResponse runnerResponse = response.getBody();
        assertNotNull(runnerResponse);
        assertTrue(runnerResponse instanceof RunnerResponse);

        List<InvoicePostingValidationResponse> responses = (List<InvoicePostingValidationResponse>) ((RunnerResponse) runnerResponse).getData();
        assertNotNull(responses);
        assertTrue(responses.isEmpty());
    }

    @Test
    void testValidateInvoicePosting_NoShipmentsFound() {
        UUID guid = UUID.randomUUID();
        request.setShipmentGuids(Collections.singletonList(guid.toString()));

        when(shipmentDao.findShipmentsByGuids(anySet())).thenReturn(Collections.emptyList());

        ResponseEntity<IRunnerResponse> response = shipmentService.validateInvoicePosting(request);

        IRunnerResponse runnerResponse = response.getBody();
        assertNotNull(runnerResponse);
        assertTrue(runnerResponse instanceof RunnerResponse);

        List<InvoicePostingValidationResponse> responses = (List<InvoicePostingValidationResponse>) ((RunnerResponse) runnerResponse).getData();
        assertNotNull(responses);
        assertTrue(responses.isEmpty());
    }

    @Test
    void fetchOrgInfoFromV1_Test() throws RunnerException {
        PartiesOrgAddressRequest request = PartiesOrgAddressRequest.builder().party(PartiesRequest.builder()
            .build()).OrgCode("og").AddressCode("ac").build();

        Mockito.doNothing().when(bookingIntegrationsUtility)
            .transformOrgAndAddressPayload(request.getParty(), request.getAddressCode(), request.getOrgCode());
        AddressDataV1 addressDataV1 = AddressDataV1.builder().build();
        OrgDataV1 orgDataV1 = OrgDataV1.builder().build();

        when(jsonHelper.convertValue(request.getParty().getAddressData(), AddressDataV1.class)).thenReturn(addressDataV1);
        when(jsonHelper.convertValue(request.getParty().getOrgData(), OrgDataV1.class)).thenReturn(orgDataV1);

        when(jsonHelper.convertValue(addressDataV1, Map.class)).thenReturn(new HashMap());
        when(jsonHelper.convertValue(orgDataV1, Map.class)).thenReturn(new HashMap());

        PartiesRequest response = shipmentService.fetchOrgInfoFromV1(request);

       assertNotNull(response);
    }

    @Test
    void testGetContainerListFromTrackingService_Success() throws RunnerException {
        Long shipmentId = 1L;
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setShipmentId("shipment-1");
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));

        Container container = new Container();
        container.setContainerBase(new ContainerBase());
        List<Container> containers = List.of(container);
        TrackingServiceApiResponse trackingResponse = new TrackingServiceApiResponse();
        trackingResponse.setContainers(containers);
        when(trackingServiceAdapter.fetchTrackingData(any(TrackingRequest.class))).thenReturn(trackingResponse);

        TrackingServiceLiteContainerResponse liteContainerResponse = new TrackingServiceLiteContainerResponse();
        liteContainerResponse.setContainers(List.of(new TrackingServiceLiteContainerResponse.LiteContainer()));
        ResponseEntity<IRunnerResponse> response = shipmentService.getContainerListFromTrackingService(shipmentId, null);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(shipmentDao).findById(shipmentId);
        verify(trackingServiceAdapter).fetchTrackingData(any(TrackingRequest.class));
    }

    @Test
    void testGetContainerListFromTrackingService_EmptyRequest() {
        assertThrows(RunnerException.class, () -> {
            shipmentService.getContainerListFromTrackingService(null, null);
        });
    }

    @Test
    void testGetContainerListFromTrackingService_ShipmentNotFound() {
        Long shipmentId = 1L;
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.empty());

        assertThrows(RunnerException.class, () -> shipmentService.getContainerListFromTrackingService(shipmentId, null));
    }

    @Test
    void testGetContainerListFromTrackingService_ConsolidationNotFound() {
        Long consolidationId = 1L;
        when(consoleShipmentMappingDao.findByConsolidationId(consolidationId)).thenReturn(List.of());

        assertThrows(RunnerException.class, () -> {
            shipmentService.getContainerListFromTrackingService(null, consolidationId);
        });
    }

    @Test
    void testGetContainerListFromTrackingService_SuccessWithConsolidationId() throws RunnerException {
        Long consolidationId = 1L;
        Long shipmentId = 2L;
        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setShipmentId(shipmentId);
        when(consoleShipmentMappingDao.findByConsolidationId(consolidationId)).thenReturn(List.of(mapping));

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setShipmentId("shipment-2");
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));

        List<Container> containers = List.of(new Container());
        TrackingServiceApiResponse trackingResponse = new TrackingServiceApiResponse();
        trackingResponse.setContainers(containers);
        when(trackingServiceAdapter.fetchTrackingData(any(TrackingRequest.class))).thenReturn(trackingResponse);

        ResponseEntity<IRunnerResponse> response = shipmentService.getContainerListFromTrackingService(null, consolidationId);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(consoleShipmentMappingDao).findByConsolidationId(consolidationId);
        verify(shipmentDao).findById(shipmentId);
        verify(trackingServiceAdapter).fetchTrackingData(any(TrackingRequest.class));
    }

    @Test
    void testGetContainerListFromTrackingService_FailedTrackingService() throws RunnerException {
        Long shipmentId = 1L;
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setShipmentId("shipment-1");
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));

        when(trackingServiceAdapter.fetchTrackingData(any(TrackingRequest.class))).thenThrow(new RuntimeException("Tracking service failed"));

        assertThrows(RunnerException.class, () -> {
            shipmentService.getContainerListFromTrackingService(shipmentId, null);
        });

        verify(shipmentDao).findById(shipmentId);
        verify(trackingServiceAdapter).fetchTrackingData(any(TrackingRequest.class));
    }

    @Test
    void create_success() throws RunnerException {
        UserContext.getUser().setPermissions(new HashMap<>());
        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setShipmentId("AIR-CAN-00001");
        AdditionalDetails additionalDetails = getmockAdditionalDetails(LocalDateTime.now(), true, true, true);
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        LocalDateTime mockDateTime = LocalDateTime.now();

        TrackingServiceApiResponse trackingResponse = new TrackingServiceApiResponse();
        trackingResponse.setContainers(List.of(Container.builder()
                .containerNumber("containerNumber")
                .journey(new Journey())
                .places(List.of())
                .events(List.of(Event.builder()
                        .eventType("eventType")
                        .actualEventTime(DateAndSources.builder()
                                .dateTime(mockDateTime).build()).build())).build()));
        TrackingServiceApiResponse.DateAndSources dateAndSources = new TrackingServiceApiResponse.DateAndSources();
        dateAndSources.setDateTime(mockDateTime);

        trackingResponse.getContainers().get(0).getJourney().setPortOfArrivalAta(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfDepartureAtd(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfArrivalEta(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfDepartureEtd(dateAndSources);

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        mockShipmentRequest.setEventsList(List.of(EventsRequest.builder()
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .eventCode("eventType").build()));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        // Mock
        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);

        Events eventType = Events.builder()
                .eventCode("eventType")
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .build();
        List<Events> eventTypeList = List.of(eventType);
        when(commonUtils.convertToEntityList(any(), eq(Events.class), any())).thenReturn(eventTypeList);

        // Test
        mockShipmentSettings();
        when(commonUtils.getCurrentTenantSettings()).thenReturn(V1TenantSettingsResponse.builder().transportModeConfig(true).build());

        when(commonUtils.isTransportModeValid(anyString(), anyString(), any())).thenReturn(true);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.create(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void create_success_trackApiError() throws RunnerException {
        UserContext.getUser().setPermissions(new HashMap<>());
        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setShipmentId("AIR-CAN-00001");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        LocalDateTime mockDateTime = LocalDateTime.now();

        TrackingServiceApiResponse trackingResponse = new TrackingServiceApiResponse();
        trackingResponse.setContainers(List.of(Container.builder()
                .containerNumber("containerNumber")
                .journey(new Journey())
                .events(List.of(Event.builder()
                        .eventType("eventType")
                        .actualEventTime(DateAndSources.builder()
                                .dateTime(mockDateTime).build()).build())).build()));
        TrackingServiceApiResponse.DateAndSources dateAndSources = new TrackingServiceApiResponse.DateAndSources();
        dateAndSources.setDateTime(mockDateTime);

        trackingResponse.getContainers().get(0).getJourney().setPortOfArrivalAta(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfDepartureAtd(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfArrivalEta(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfDepartureEtd(dateAndSources);

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        mockShipmentRequest.setEventsList(List.of(EventsRequest.builder()
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .eventCode("eventType").build()));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        // Mock
        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);

        Events eventType = Events.builder()
                .eventCode("eventType")
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .build();
        List<Events> eventTypeList = List.of(eventType);
        when(commonUtils.convertToEntityList(any(), eq(Events.class), any())).thenReturn(eventTypeList);

        // Test
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.create(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void create_success_trackApiNull() throws RunnerException {
        UserContext.getUser().setPermissions(new HashMap<>());
        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setShipmentId("AIR-CAN-00001");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        LocalDateTime mockDateTime = LocalDateTime.now();

        TrackingServiceApiResponse trackingResponse = new TrackingServiceApiResponse();
        trackingResponse.setContainers(List.of(Container.builder()
                .containerNumber("containerNumber")
                .journey(new Journey())
                .events(List.of(Event.builder()
                        .eventType("eventType")
                        .actualEventTime(DateAndSources.builder()
                                .dateTime(mockDateTime).build()).build())).build()));
        TrackingServiceApiResponse.DateAndSources dateAndSources = new TrackingServiceApiResponse.DateAndSources();
        dateAndSources.setDateTime(mockDateTime);

        trackingResponse.getContainers().get(0).getJourney().setPortOfArrivalAta(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfDepartureAtd(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfArrivalEta(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfDepartureEtd(dateAndSources);

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        mockShipmentRequest.setEventsList(List.of(EventsRequest.builder()
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .eventCode("eventType").build()));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        // Mock
        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);

        Events eventType = Events.builder()
                .eventCode("eventType")
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .build();
        List<Events> eventTypeList = List.of(eventType);
        when(commonUtils.convertToEntityList(any(), eq(Events.class), any())).thenReturn(eventTypeList);

        // Test
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.create(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void create_success_emptyActualEventDate() throws RunnerException {
        UserContext.getUser().setPermissions(new HashMap<>());
        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setShipmentId("AIR-CAN-00001");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        LocalDateTime mockDateTime = LocalDateTime.now();

        TrackingServiceApiResponse trackingResponse = new TrackingServiceApiResponse();
        trackingResponse.setContainers(List.of(Container.builder()
                .containerNumber("containerNumber")
                .journey(new Journey())
                .places(List.of())
                .events(List.of(Event.builder()
                        .eventType("eventType")
                        .build())).build()));
        TrackingServiceApiResponse.DateAndSources dateAndSources = new TrackingServiceApiResponse.DateAndSources();
        dateAndSources.setDateTime(mockDateTime);

        trackingResponse.getContainers().get(0).getJourney().setPortOfArrivalAta(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfDepartureAtd(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfArrivalEta(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfDepartureEtd(dateAndSources);

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        mockShipmentRequest.setEventsList(List.of(EventsRequest.builder()
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .eventCode("eventType").build()));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        // Mock
        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);

        Events eventType = Events.builder()
                .eventCode("eventType")
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .build();
        List<Events> eventTypeList = List.of(eventType);
        when(commonUtils.convertToEntityList(any(), eq(Events.class), any())).thenReturn(eventTypeList);

        // Test
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.create(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeUpdate_success() throws RunnerException {
        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = shipmentDetails;
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

    // Mock
        when(shipmentDao.findById(any()))
            .thenReturn(
                Optional.of(
                    shipmentDetails
                        .setConsolidationList(new HashSet<>())
                        .setContainersList(new HashSet<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);

        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        mockShipmentSettings();
//        mockTenantSettings();
        when(commonUtils.getCurrentTenantSettings()).thenReturn(V1TenantSettingsResponse.builder().transportModeConfig(true).build());

//        when(commonUtils.isTransportModeValid(anyString(), anyString(), any())).thenReturn(true);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }



    @ParameterizedTest
    @MethodSource("shipmentTypeAndB2bProvider")
    void completeUpdate_withMultipleShipmentTypesAndB2bValues_success(String shipmentType, Boolean b2b) throws RunnerException {
        shipmentDetails.setId(1L);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        // Convert ShipmentDetails to ShipmentRequest and apply test case
        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(shipmentDetails, ShipmentRequest.class);
        mockShipmentRequest.setShipmentType(shipmentType);
        mockShipmentRequest.setB2b(b2b);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);

        // Mock behaviors
        when(shipmentDao.findById(any()))
                .thenReturn(Optional.of(
                        shipmentDetails
                                .setConsolidationList(new HashSet<>())
                                .setContainersList(new HashSet<>())
                ));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        when(commonUtils.getCurrentTenantSettings())
                .thenReturn(V1TenantSettingsResponse.builder().transportModeConfig(true).build());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setEnableRouteMaster(true);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());

        // Test the method
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        // Verify response
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    private static Stream<Arguments> shipmentTypeAndB2bProvider() {
        return Stream.of(
                Arguments.of("HSE", false),  // Matches: HSE and not b2b
                Arguments.of("HSE", true),   // Matches: b2b is true
                Arguments.of("SCN", false), // Matches: SCN
                Arguments.of("SCN", true),  // Matches: SCN
                Arguments.of("BCN", false), // Matches: BCN
                Arguments.of("BCN", true),  // Matches: BCN
                Arguments.of("DRT", false), // Matches: DRT
                Arguments.of("DRT", true),  // Matches: DRT
                Arguments.of("OTH", false) // Does not match any condition
        );
    }



    @Test
    void completeUpdate_success_validMasterBill() throws RunnerException {
        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = shipmentDetails;
        shipmentDetails.setMasterBill("sampleMasterBill");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        ConsolidationDetailsProjection consolidationDetailsProjection = new ConsolidationDetailsProjection() {
            @Override
            public Integer getTenantId() { return 3; }
            @Override
            public String getConsolidationNumber() { return "CON456"; }
            @Override
            public String getMawb() { return "MAWB456"; }
            @Override
            public String getBol() { return "BOL456"; }
        };

        // Mock
        when(shipmentDao.findById(any()))
                .thenReturn(
                        Optional.of(
                                shipmentDetails
                                        .setConsolidationList(new HashSet<>())
                                        .setContainersList(new HashSet<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        mockShipmentSettings();
        mockTenantSettings();
        when(consolidationDetailsDao.findMblNumberInDifferentTenant(shipmentDetails.getMasterBill())).thenReturn(List.of(consolidationDetailsProjection));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeUpdate_efreightStausChanged() throws RunnerException {
        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = objectMapper.convertValue(shipmentDetails, ShipmentDetails.class);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        mockShipment.getAdditionalDetails().setEfreightStatus("new value");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        // Mock
        when(shipmentDao.findById(any()))
            .thenReturn(
                Optional.of(
                    shipmentDetails
                        .setConsolidationList(new HashSet<>())
                        .setContainersList(new HashSet<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        mockShipmentSettings();
        mockTenantSettings();
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        verify(awbDao, times(1)).updatedAwbInformationEvent(any(ShipmentDetails.class), any(ShipmentDetails.class));
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }
    @Test
    void completeUpdate_SciChanged() throws RunnerException {
        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = objectMapper.convertValue(shipmentDetails, ShipmentDetails.class);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        mockShipment.getAdditionalDetails().setSci("T1");
        mockShipment.getAdditionalDetails().setEfreightStatus("new value");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        // Mock
        when(shipmentDao.findById(any()))
                .thenReturn(
                        Optional.of(
                                shipmentDetails
                                        .setConsolidationList(new HashSet<>())
                                        .setContainersList(new HashSet<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        mockShipmentSettings();
        mockTenantSettings();
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        verify(awbDao, times(1)).updatedAwbInformationEvent(any(ShipmentDetails.class), any(ShipmentDetails.class));
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeUpdate_Failure() throws RunnerException {
        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = shipmentDetails;
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.getAdditionalDetails().setPrintedOriginal(true);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setConsolidationList(new HashSet<>()).setContainersList(new HashSet<>());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetails oldEntity = jsonTestUtility.getTestShipment();
        oldEntity.setId(1L);
        oldEntity.setConsolidationList(new HashSet<>()).setContainersList(new HashSet<>());

        // Mock
        when(shipmentDao.findById(any())).thenReturn(Optional.of(oldEntity));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        // Test
        assertThrows(ValidationException.class, () -> shipmentService.completeUpdate(commonRequestModel));
    }


    @Test
    void toggleLock_lock() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);
        ShipmentDetails mockShipment = shipmentDetails;

        Mockito.when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        Mockito.when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment.setIsLocked(true).setLockedBy("user"));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.toggleLock(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
        verify(shipmentSync, times(1)).syncLockStatus(any());

    }

    @Test
    void toggleLock_unlock() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setIsLocked(true);
        mockShipment.setLockedBy("user");

        Mockito.when(shipmentDao.findById(1L)).thenReturn(Optional.of(mockShipment));
        Mockito.when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment.setIsLocked(false));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.toggleLock(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
        verify(shipmentSync, times(1)).syncLockStatus(any());
    }

    @Test
    void cloneShipment() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        // Mock
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(
                objectMapper.convertValue(shipmentDetails, ShipmentRequest.class));

        ShipmentDetails mockShip = shipmentDetails;
        mockShip.setHouseBill(null);
        mockShip.setBookingNumber(null);
        mockShip.setContainersList(null);
        mockShip.setRoutingsList(null);
        mockShip.setShipmentId(null);
        mockShip.setMasterBill(null);
        mockShip.setConsolidationList(null);
        mockShip.setStatus(ShipmentStatus.Created.getValue());
        mockShip.setConsolRef(null);
        mockShip.setEventsList(null);
        mockShip.setShipmentCreatedOn(LocalDateTime.now());

        ShipmentDetailsResponse mockShipResponse = objectMapper.convertValue(mockShip, ShipmentDetailsResponse.class);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipResponse);

        //Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.cloneShipment(commonRequestModel);
        //Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipResponse), httpResponse);
    }

    @Test
    void cloneShipment_nullPacks() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        // Mock
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        shipmentDetails.setPackingList(null);
        when(jsonHelper.convertValue(any(), eq(ShipmentRequest.class))).thenReturn(
                objectMapper.convertValue(shipmentDetails, ShipmentRequest.class));

        ShipmentDetails mockShip = shipmentDetails;
        mockShip.setHouseBill(null);
        mockShip.setBookingNumber(null);
        mockShip.setContainersList(null);
        mockShip.setRoutingsList(null);
        mockShip.setShipmentId(null);
        mockShip.setMasterBill(null);
        mockShip.setConsolidationList(null);
        mockShip.setStatus(ShipmentStatus.Created.getValue());
        mockShip.setConsolRef(null);
        mockShip.setEventsList(null);
        mockShip.setPackingList(null);
        mockShip.setShipmentCreatedOn(LocalDateTime.now());

        ShipmentDetailsResponse mockShipResponse = objectMapper.convertValue(mockShip, ShipmentDetailsResponse.class);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipResponse);

        //Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.cloneShipment(commonRequestModel);
        //Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipResponse), httpResponse);
    }

    @Test
    void generateCustomHouseBL_restrictHblGen() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().restrictHblGen(true).build());
        mockShipmentSettings();
        String hbl = shipmentService.generateCustomHouseBL(null);
        String mockHbl = null;

        assertEquals(mockHbl, hbl);
    }

    @Test
    void generateCustomHouseBL_product_sequence() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().customisedSequence(true).build());

        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setHouseBill(null);
        String mockHbl = "hblPrefix-hblSuffix-001";

        // Mock
        when(productEngine.getCustomizedBLNumber(any())).thenReturn(mockHbl);
        mockShipmentSettings();
        // Test
        String hbl = shipmentService.generateCustomHouseBL(mockShipment);

        assertEquals(mockHbl, hbl);
    }

    @Test
    void generateCustomHouseBL_random() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder()
                .customisedSequence(false).housebillNumberGeneration("Random").build());

        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setHouseBill(null);
        mockShipmentSettings();
        // Test
        String hbl = shipmentService.generateCustomHouseBL(mockShipment);

        assertNotNull(hbl);
        assertEquals(10, hbl.length());
    }

    @Test
    void generateCustomHouseBL_serial() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder()
                .customisedSequence(false).housebillNumberGeneration("Serial").build());

        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setHouseBill(null);

        //Mock
        when(v1Service.getShipmentSerialNumber()).thenReturn("112344");

        mockShipmentSettings();
        // Test
        String hbl = shipmentService.generateCustomHouseBL(mockShipment);

        assertNotNull(hbl);
        assertEquals("112344", hbl);
    }

    @Test
    void getDefaultShipment() {
        // Mock data
        ShipmentSettingsDetails tenantSettings = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        UsersDto user = new UsersDto();
        user.setTenantId(1);
        UserContext.setUser(user);

        TenantModel tenantModel = new TenantModel();
        V1RetrieveResponse mockTenantResponse = new V1RetrieveResponse();
        mockTenantResponse.setEntity(tenantModel);
        when(v1Service.retrieveTenant()).thenReturn(mockTenantResponse);

        LocalDateTime mockDateTime = LocalDateTime.now();

        ShipmentDetailsResponse expectedResponse = new ShipmentDetailsResponse();
        expectedResponse.setSource(Constants.SYSTEM);
        expectedResponse.setStatus(ShipmentStatus.Created.getValue());
        expectedResponse.setAdditionalDetails(new AdditionalDetailResponse());
        expectedResponse.setCarrierDetails(new CarrierDetailResponse());
        expectedResponse.setCustomerCategory(CustomerCategoryRates.CATEGORY_5);
        expectedResponse.setShipmentCreatedOn(mockDateTime);
        expectedResponse.setSourceTenantId(1L);

        // Mocking behavior of dependencies
//        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
//        when(jsonHelper.buildSuccessResponse(any())).thenReturn(ResponseHelper.buildSuccessResponse(expectedResponse));

        // Execute the method under test
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getDefaultShipment();
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
//        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(runnerResponse.getData(), ShipmentDetailsResponse.class);
//        shipmentDetailsResponse.setShipmentCreatedOn(mockDateTime);
//        assertEquals(expectedResponse.getSourceTenantId(), shipmentDetailsResponse.getSourceTenantId());

        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void getDefaultShipmentPopulatesDefaultDepartmentFromMdm() {
        // Mock data
        ShipmentSettingsDetails tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setDefaultTransportMode("AIR");
        tenantSettings.setDefaultShipmentType("EXP");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        UsersDto user = new UsersDto();
        user.setTenantId(1);
        UserContext.setUser(user);

        TenantModel tenantModel = new TenantModel();
        V1RetrieveResponse mockTenantResponse = new V1RetrieveResponse();
        mockTenantResponse.setEntity(tenantModel);
        when(v1Service.retrieveTenant()).thenReturn(mockTenantResponse);

        LocalDateTime mockDateTime = LocalDateTime.now();

        ShipmentDetailsResponse expectedResponse = new ShipmentDetailsResponse();
        expectedResponse.setSource(Constants.SYSTEM);
        expectedResponse.setStatus(ShipmentStatus.Created.getValue());
        expectedResponse.setAdditionalDetails(new AdditionalDetailResponse());
        expectedResponse.setCarrierDetails(new CarrierDetailResponse());
        expectedResponse.setCustomerCategory(CustomerCategoryRates.CATEGORY_5);
        expectedResponse.setShipmentCreatedOn(mockDateTime);
        expectedResponse.setSourceTenantId(1L);

        when(commonUtils.getAutoPopulateDepartment(anyString(), anyString(), anyString())).thenReturn("AE");
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);

        // Execute the method under test
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getDefaultShipment();
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(runnerResponse.getData(), ShipmentDetailsResponse.class);

        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals("AE", shipmentDetailsResponse.getDepartment());
    }


    @Test
    void calculateContainerSummary_throws_exception() throws RunnerException {
        CalculateContainerSummaryRequest containerSummaryRequest = new CalculateContainerSummaryRequest();
        List<ContainerRequest> containerRequests = new ArrayList<>();
        containerSummaryRequest.setContainersList(containerRequests);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(containerSummaryRequest);
        ContainerSummaryResponse containerSummaryResponse = new ContainerSummaryResponse();
        String errorMessage = "error while calculating summary !";
        // Mock
        when(containerService.calculateContainerSummary(anyList(), any(), any()))
                .thenThrow(new RunnerException(errorMessage));
        // Test
        ResponseEntity<IRunnerResponse> httpResponse =  shipmentService.calculateContainerSummary(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        RunnerResponse runnerResponse  = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void calculateContainerSummary_success() throws RunnerException {
        CalculateContainerSummaryRequest containerSummaryRequest = new CalculateContainerSummaryRequest();
        List<ContainerRequest> containerRequests = new ArrayList<>();
        containerSummaryRequest.setContainersList(containerRequests);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(containerSummaryRequest);

        ContainerSummaryResponse containerSummaryResponse = new ContainerSummaryResponse();
        // Mock
        when(containerService.calculateContainerSummary(anyList(), any(), any())).thenReturn(containerSummaryResponse);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse =  shipmentService.calculateContainerSummary(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(containerSummaryResponse), httpResponse);

    }

    @Test
    void calculatePackSummary_throws_exception() throws RunnerException {
        CalculatePackSummaryRequest calculatePackSummaryRequest = new CalculatePackSummaryRequest();
        List<PackingRequest> packingRequests = new ArrayList<>();
        calculatePackSummaryRequest.setPackingList(packingRequests);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(calculatePackSummaryRequest);
        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        String errorMessage = "error while calculating summary !";
        // Mock
        when(packingService.calculatePackSummary(anyList(), any(), any(),any()))
                .thenThrow(new RunnerException(errorMessage));
        // Test
        ResponseEntity<IRunnerResponse> httpResponse =  shipmentService.calculatePackSummary(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        RunnerResponse runnerResponse  = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void calculatePackSummary_success() throws RunnerException {
        CalculatePackSummaryRequest calculatePackSummaryRequest = new CalculatePackSummaryRequest();
        List<PackingRequest> packingRequests = new ArrayList<>();
        calculatePackSummaryRequest.setPackingList(packingRequests);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(calculatePackSummaryRequest);

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        // Mock
        when(packingService.calculatePackSummary(anyList(), any(), any(),any())).thenReturn(packSummaryResponse);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse =  shipmentService.calculatePackSummary(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(packSummaryResponse), httpResponse);
    }


    @Test
    void getIdFromGuid_success() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        when(shipmentDao.findByGuid(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06"))).thenReturn(Optional.of(shipmentDetails));
        ShipmentDetailsResponse mockShipmentResponse = ShipmentDetailsResponse.builder().id(
                shipmentDetails.getId()
        ).build();

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getIdFromGuid(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void getGuidFromId_success() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        ShipmentDetailsResponse mockShipmentResponse = ShipmentDetailsResponse.builder().guid(
                shipmentDetails.getGuid()
        ).build();

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getGuidFromId(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }


    @Test
    void retrieveByOrderId_success() throws RunnerException {
        String mockOrder = "test_order_id";
        ShipmentDetailsResponse mockShipResponse = objectMapper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
        //Mock
        when(orderManagementAdapter.getOrder(mockOrder)).thenReturn(shipmentDetails);
        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipResponse);
        when(masterDataHelper.addAllMasterDataInSingleCall(any(), any())).thenReturn(null);
        when(masterDataHelper.addAllUnlocationDataInSingleCall(any(), any())).thenReturn(null);

        //Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.retrieveByOrderId(mockOrder);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipResponse), httpResponse);
    }

    @Test
    void containerListForTI_fails_when_request_is_null() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.containerListForTI(commonRequestModel);
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);

        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, runnerResponse.getError().getMessage());
    }

    @Test
    void containerListForTI_fails_when_shipment_doesnt_exist() {
        TIContainerListRequest tiContainerListRequest = new TIContainerListRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(tiContainerListRequest);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.containerListForTI(commonRequestModel);
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);

        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, runnerResponse.getError().getMessage());
    }

    @Test
    void updateDateAndStatus_success() throws RunnerException {
        ShipmentDetails mockShipment = shipmentDetails;

        LocalDateTime mockDateTime = LocalDateTime.now();
        Integer mockStatus = 2;
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(mockShipment));

        // Update with inputs
        mockShipment.getAdditionalDetails().setDateOfIssue(mockDateTime);
        mockShipment.setStatus(5);
        // Mock
        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>()));
        when(shipmentDao.save(eq(mockShipment), eq(false))).thenReturn(mockShipment);
        // Test
        shipmentService.updateDateAndStatus(1L, mockDateTime, mockStatus);
        // Assert
        verify(shipmentDao, times(1)).save(any(), anyBoolean());
        verify(shipmentSync, times(1)).sync(any(), eq(null), eq(null), any(), eq(false));

    }

    @Test
    void transportInstructionList_fails_on_empty_request() {
        TIListRequest tiListRequest = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(tiListRequest);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.transportInstructionList(commonRequestModel);
        // Assert
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, runnerResponse.getError().getMessage());
    }

    @Test
    void transportInstructionList_fails_on_empty_shipmentGuid() {
        TIListRequest tiListRequest = new TIListRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(tiListRequest);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.transportInstructionList(commonRequestModel);
        // Assert
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, runnerResponse.getError().getMessage());
    }

    @Test
    void transportInstructionList_success() {
        TIListRequest tiListRequest = new TIListRequest();
        tiListRequest.setShipmentGuid(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06"));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(tiListRequest);

        // Mock
        when(v1Service.fetchTransportInstructionList(tiListRequest)).thenReturn(new V1DataResponse());
        List<TIResponse> mockTIResponse = List.of(new TIResponse());
        when(jsonHelper.convertValueToList(any(), eq(TIResponse.class))).thenReturn(mockTIResponse);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.transportInstructionList(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockTIResponse), httpResponse);
    }


    @Test
    void containerListForTI_success() {
        TIContainerListRequest tiContainerListRequest = new TIContainerListRequest();
        tiContainerListRequest.setShipmentGuid(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06"));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(tiContainerListRequest);

        // Mock
        V1DataResponse mockV1DataResponse = new V1DataResponse();
        when(v1Service.fetchContainersListForTI(eq(tiContainerListRequest))).thenReturn(mockV1DataResponse);
        List<TIContainerResponse> mockContainerResponseList = new ArrayList<>();
        when(jsonHelper.convertValueToList(any(), eq(TIContainerResponse.class))).thenReturn(mockContainerResponseList);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.containerListForTI(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockContainerResponseList), httpResponse);
    }

    @Test
    void getShipmentFromConsol_fails_when_consol_is_not_present() {
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.empty());

        Long consolidationId = 100L;

        Exception e = assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentService.getShipmentFromConsol(consolidationId, "");
        });

        String errorMessage ="Failed to fetch the consolidation with id " + consolidationId;
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void getShipmentFromConsol_success() {

        Long consolidationId = 1L;
        String bookingNumber = "bookingNumber";
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().restrictHblGen(true).build());
        ConsolidationDetailsResponse consolidationDetailsResponse = objectMapper.convertValue(testConsol, ConsolidationDetailsResponse.class);
        ConsolidationListResponse consolidationListResponse = objectMapper.convertValue(testConsol, ConsolidationListResponse.class);


        // Mock
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(testConsol));
        when(modelMapper.map(any(), eq(ConsolidationDetailsResponse.class))).thenReturn(consolidationDetailsResponse);
        when(modelMapper.map(any(), eq(ConsolidationListResponse.class))).thenReturn(consolidationListResponse);
        when(jsonHelper.convertValue(any(), eq(PartiesResponse.class))).thenReturn(new PartiesResponse());
        //when(modelMapper.map(any(), eq(RoutingsResponse.class))).thenReturn(new RoutingsResponse());
        mockShipmentSettings();
        // Test
        SpringContext.setApplicationContext(applicationContext);
        Mockito.when(applicationContext.getBean(CommonUtils.class)).thenReturn(commonUtils);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getShipmentFromConsol(consolidationId, bookingNumber);
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        ShipmentDetailsResponse shipmentResponse = objectMapper.convertValue(runnerResponse.getData(), ShipmentDetailsResponse.class);
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(Constants.SYSTEM, shipmentResponse.getSource());
        assertEquals(Constants.SHIPMENT_TYPE_STD, shipmentResponse.getJobType());
        assertEquals(bookingNumber, shipmentResponse.getBookingNumber());
    }

    @Test
    void getShipmentFromConsol_success_SEA_FCL() {

        Long consolidationId = 1L;
        String bookingNumber = "bookingNumber";
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().restrictHblGen(true).build());
        testConsol.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        testConsol.setShipmentsList(Set.of(shipmentDetails));

        ConsolidationDetailsResponse consolidationDetailsResponse = objectMapper.convertValue(testConsol, ConsolidationDetailsResponse.class);
        ConsolidationListResponse consolidationListResponse = objectMapper.convertValue(testConsol, ConsolidationListResponse.class);

        // Mock
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(testConsol));
        when(modelMapper.map(any(), eq(ConsolidationDetailsResponse.class))).thenReturn(consolidationDetailsResponse);
        when(modelMapper.map(any(), eq(ConsolidationListResponse.class))).thenReturn(consolidationListResponse);
        when(jsonHelper.convertValue(any(), eq(PartiesResponse.class))).thenReturn(new PartiesResponse());
        //when(modelMapper.map(any(), eq(RoutingsResponse.class))).thenReturn(new RoutingsResponse());
        mockShipmentSettings();
        // Test
        SpringContext.setApplicationContext(applicationContext);
        Mockito.when(applicationContext.getBean(CommonUtils.class)).thenReturn(commonUtils);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getShipmentFromConsol(consolidationId, bookingNumber);
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        ShipmentDetailsResponse shipmentResponse = objectMapper.convertValue(runnerResponse.getData(), ShipmentDetailsResponse.class);
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(Constants.SYSTEM, shipmentResponse.getSource());
        assertEquals(Constants.SHIPMENT_TYPE_STD, shipmentResponse.getJobType());
        assertEquals(bookingNumber, shipmentResponse.getBookingNumber());
    }
    @Test
    void getShipmentFromConsol_success_SEA_LCL() {

        Long consolidationId = 1L;
        String bookingNumber = "bookingNumber";
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().restrictHblGen(true).build());
        testConsol.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        testConsol.setShipmentsList(Set.of(shipmentDetails));

        ConsolidationDetailsResponse consolidationDetailsResponse = objectMapper.convertValue(testConsol, ConsolidationDetailsResponse.class);
        ConsolidationListResponse consolidationListResponse = objectMapper.convertValue(testConsol, ConsolidationListResponse.class);

        // Mock
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(testConsol));
        when(modelMapper.map(any(), eq(ConsolidationDetailsResponse.class))).thenReturn(consolidationDetailsResponse);
        when(modelMapper.map(any(), eq(ConsolidationListResponse.class))).thenReturn(consolidationListResponse);
        when(jsonHelper.convertValue(any(), eq(PartiesResponse.class))).thenReturn(new PartiesResponse());
        //when(modelMapper.map(any(), eq(RoutingsResponse.class))).thenReturn(new RoutingsResponse());
        mockShipmentSettings();
        // Test
        SpringContext.setApplicationContext(applicationContext);
        Mockito.when(applicationContext.getBean(CommonUtils.class)).thenReturn(commonUtils);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getShipmentFromConsol(consolidationId, bookingNumber);
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        ShipmentDetailsResponse shipmentResponse = objectMapper.convertValue(runnerResponse.getData(), ShipmentDetailsResponse.class);
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(Constants.SYSTEM, shipmentResponse.getSource());
        assertEquals(Constants.SHIPMENT_TYPE_STD, shipmentResponse.getJobType());
        assertEquals(bookingNumber, shipmentResponse.getBookingNumber());
    }


    @Test
    void getMasterDataMappings_success() throws RunnerException, NoSuchFieldException, ClassNotFoundException, IllegalAccessException {
        TenantContext.setCurrentTenant(1);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        List<MasterDataDescriptionResponse> mockResponse = new ArrayList<>();

        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(masterDataUtils.getMasterDataDescription(any())).thenReturn(mockResponse);


        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getMasterDataMappings();

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);

    }

    @Test
    void attachListShipment_success() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(true); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(false);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(new CarrierDetails());
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        mockShipmentSettings();
        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentService.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void attachListShipment_success_SEA_FCL() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(true); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(false);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        consolidationDetails.setShipmentsList(Set.of(shipmentDetails));
        consolidationDetails.setCarrierDetails(new CarrierDetails());
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        mockShipmentSettings();
        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentService.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void attachListShipment_success_SEA_LCL() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(true); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(false);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        consolidationDetails.setShipmentsList(Set.of(shipmentDetails));
        consolidationDetails.setCarrierDetails(new CarrierDetails());
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        mockShipmentSettings();
        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentService.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void attachListShipmentEtaNotNull_success() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(false); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(true);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(CarrierDetails.builder().originPort(Constants.ORIGIN_PORT).destinationPort(Constants.DESTINATION_PORT).eta(LocalDateTime.now()).etd(LocalDateTime.now()).vessel(Constants.VESSEL).voyage(Constants.VOYAGE).build());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );

        mockShipmentSettings();
        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentService.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void attachListShipmentEtaNotNullTransportModeAir_success() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(false); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(true);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(CarrierDetails.builder().eta(LocalDateTime.now()).etd(LocalDateTime.now()).shippingLine(Constants.SHIPPING_LINE).flightNumber(Constants.FLIGHT_NUMBER).build());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirDGFlag(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        mockShipmentSettings();
        mockTenantSettings();
        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentService.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void attachListShipmentEtaNotNullTransportModeAir_EXP_success() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(true);
        attachListShipmentRequest.setEtaMatch(true);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(CarrierDetails.builder().eta(LocalDateTime.now()).etd(LocalDateTime.now()).shippingLine(Constants.SHIPPING_LINE).flightNumber(Constants.FLIGHT_NUMBER).build());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(Constants.DIRECTION_EXP);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirDGFlag(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        TenantSettingsDetailsContext.getCurrentTenantSettings().setIsMAWBColoadingEnabled(true);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationIdAll(anyLong())).thenReturn(List.of(ConsoleShipmentMapping.builder().consolidationId(2L).shipmentId(3L).build()));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        mockShipmentSettings();
        mockTenantSettings();
        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentService.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void attachListShipmentEtaNotNullTransportModeAir_success_dg_user() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(false); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(true);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(CarrierDetails.builder().eta(LocalDateTime.now()).etd(LocalDateTime.now()).shippingLine(Constants.SHIPPING_LINE).flightNumber(Constants.FLIGHT_NUMBER).build());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirDGFlag(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        UserContext.getUser().getPermissions().put(PermissionConstants.airDG, true);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        mockShipmentSettings();
        mockTenantSettings();
        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentService.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void attachListShipmentEtaNotNullTransportModeAir_success_hazardous_console() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(false); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(true);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(CarrierDetails.builder().eta(LocalDateTime.now()).etd(LocalDateTime.now()).shippingLine(Constants.SHIPPING_LINE).flightNumber(Constants.FLIGHT_NUMBER).build());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setHazardous(true);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirDGFlag(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        mockShipmentSettings();
        mockTenantSettings();

        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentService.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void attachListShipmentEtaNotNullTransportModeNotAir_success() {
        // Mock data
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(false); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(true);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(CarrierDetails.builder().eta(LocalDateTime.now()).etd(LocalDateTime.now()).shippingLine(Constants.SHIPPING_LINE).flightNumber(Constants.FLIGHT_NUMBER).build());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirDGFlag(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        // Set up shipmentDetailsList as needed for your test

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        mockShipmentSettings();
        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentService.attachListShipment(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
    }

    @Test
    void fetchShipmentsForConsoleId_success() throws RunnerException {
        Long consoleId = 1L;
        CommonGetRequest request = CommonGetRequest.builder().id(consoleId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        List<ConsoleShipmentMapping> consoleShipmentMappings = new ArrayList<>();
        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setShipmentId(1L);
        mapping.setConsolidationId(1L);
        consoleShipmentMappings.add(mapping);
        shipmentDetails.setEventsList(null);
        // Set up consoleShipmentMappings as needed for your test
        when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(consoleShipmentMappings);

        List<ShipmentDetails> shipments = List.of(shipmentDetails);
        List<IRunnerResponse> shipmentResponse = convertEntityListToDtoList(shipments);
        PageImpl page = new PageImpl(List.of(shipmentDetails));

        ResponseEntity<IRunnerResponse> expectedResponse = ResponseHelper.buildListSuccessResponse(
                shipmentResponse,
                page.getTotalPages(),
                page.getTotalElements()
        );

        when(shipmentDao.findAll(any(), any())).thenReturn(page);
        // Moved the below mocking part to convertEntityListToDtoList method
//        when(modelMapper.map(any(), eq(ShipmentListResponse.class))).thenReturn(objectMapper.convertValue(shipmentDetails, ShipmentListResponse.class));

        // Execute the method under test
        ResponseEntity<IRunnerResponse> result = shipmentService.fetchShipmentsForConsoleId(commonRequestModel);


        assertEquals(expectedResponse, result);
    }

    @Test
    void fetchActiveInvoices_throws_exception() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        String errorMessage = "Shipment Guid can't be null";

        Exception e = assertThrows(RunnerException.class, () -> shipmentService.fetchActiveInvoices(commonRequestModel));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void fetchActiveInvoices_success() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        InvoiceSummaryRequest invoiceSummaryRequest = new InvoiceSummaryRequest();
        invoiceSummaryRequest.setModuleType("SHIPMENT");
        invoiceSummaryRequest.setModuleGuid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06");

        when(billingServiceAdapter.fetchActiveInvoices(any())).thenReturn(false);
        CheckActiveInvoiceResponse checkActiveInvoiceResponse = CheckActiveInvoiceResponse.builder().IsAnyActiveInvoiceFound(false).build();

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fetchActiveInvoices(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(checkActiveInvoiceResponse), httpResponse);
    }

    @Test
    void fetchActiveInvoicesDoubleValuePresent() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        InvoiceSummaryRequest invoiceSummaryRequest = new InvoiceSummaryRequest();
        invoiceSummaryRequest.setModuleType("SHIPMENT");
        invoiceSummaryRequest.setModuleGuid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06");

        when(billingServiceAdapter.fetchActiveInvoices(any())).thenReturn(true);
        CheckActiveInvoiceResponse checkActiveInvoiceResponse = CheckActiveInvoiceResponse.builder().IsAnyActiveInvoiceFound(true).build();

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fetchActiveInvoices(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(checkActiveInvoiceResponse), httpResponse);
    }

    @Test
    void showAssignAllContainers_success_shows_dialog() {
        ShipmentConsoleIdDto request = new ShipmentConsoleIdDto();
        request.setShipmentId(1L);
        request.setConsolidationId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        AssignAllDialogDto mockResponse = new AssignAllDialogDto();
        mockResponse.setShowDialog(true);
        mockResponse.setNumberOfShipments(1);

        // Mock
        ShipmentsContainersMapping mockShipContainerMapping = new ShipmentsContainersMapping();
        mockShipContainerMapping.setShipmentId(1L);
        mockShipContainerMapping.setContainerId(1L);
        List<ShipmentsContainersMapping> mockShipContainerList = List.of(mockShipContainerMapping);

        ConsoleShipmentMapping mockConsolShipMapping = new ConsoleShipmentMapping();
        mockConsolShipMapping.setConsolidationId(1L);
        mockConsolShipMapping.setShipmentId(1L);
        List<ConsoleShipmentMapping> mockConsolShipMappingList = List.of(mockConsolShipMapping);

        Containers consolContainer1 = Containers.builder().consolidationId(1L).build();
        consolContainer1.setId(2L);
        Containers consolContainer2 = Containers.builder().consolidationId(1L).build();
        consolContainer2.setId(2L);

        when(shipmentsContainersMappingDao.findByShipmentId(eq(1L))).thenReturn(mockShipContainerList);
        when(containerDao.findByConsolidationId(1L)).thenReturn(List.of(consolContainer1, consolContainer2));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(List.of(
                ShipmentSettingsDetails.builder().build()));
        when(consoleShipmentMappingDao.findByConsolidationId(eq(1L))).thenReturn(mockConsolShipMappingList);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.showAssignAllContainers(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);

    }

    @Test
    void showAssignAllContainers_success_doesnt_show_dialog() {
        ShipmentConsoleIdDto request = new ShipmentConsoleIdDto();
        request.setShipmentId(1L);
        request.setConsolidationId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        AssignAllDialogDto mockResponse = new AssignAllDialogDto();
        mockResponse.setShowDialog(false);
        mockResponse.setNumberOfShipments(1);

        // Mock
        ShipmentsContainersMapping mockShipContainerMapping = new ShipmentsContainersMapping();
        mockShipContainerMapping.setShipmentId(1L);
        mockShipContainerMapping.setContainerId(1L);
        List<ShipmentsContainersMapping> mockShipContainerList = List.of(mockShipContainerMapping);

        ConsoleShipmentMapping mockConsolShipMapping = new ConsoleShipmentMapping();
        mockConsolShipMapping.setConsolidationId(1L);
        mockConsolShipMapping.setShipmentId(1L);
        List<ConsoleShipmentMapping> mockConsolShipMappingList = List.of(mockConsolShipMapping);

        Containers consolcontainer = Containers.builder().consolidationId(1L).build();
        consolcontainer.setId(2L);

        when(shipmentsContainersMappingDao.findByShipmentId(eq(1L))).thenReturn(mockShipContainerList);
        when(containerDao.findByConsolidationId(1L)).thenReturn(List.of(consolcontainer));
        when(consoleShipmentMappingDao.findByConsolidationId(eq(1L))).thenReturn(mockConsolShipMappingList);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.showAssignAllContainers(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);

    }

    @Test
    void fetchCreditLimit_throws_exception_on_empty_orgcode() {
        String errorMessage = "OrgCode to fetch creditLimit can't be null";
        Exception e = assertThrows(RunnerException.class, () -> {
            shipmentService.fetchCreditLimit(null, null);
        });

        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void fetchCreditLimit_success() throws RunnerException {
        String orgCode = "ORG123";
        String addressCode = "Default1";

        //Mock
        V1DataResponse mockV1Response = new V1DataResponse();
        CreditLimitResponse mockCreditLimitResponse = new CreditLimitResponse();
        mockV1Response.setEntities(List.of(mockCreditLimitResponse));
        when(v1Service.fetchCreditLimit(any())).thenReturn(mockV1Response);
        when(jsonHelper.convertValueToList(any(), eq(CreditLimitResponse.class))).thenReturn(List.of(mockCreditLimitResponse));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fetchCreditLimit(orgCode, addressCode);
        // Assert
        assertEquals(ResponseHelper.buildDependentServiceResponse(mockCreditLimitResponse, 0, 0), httpResponse);
    }


    @Test
    void checkCreditLimitFromV1_empty_shipment_id() {
        CheckCreditLimitFromV1Request checkCreditLimitFromV1Request = CheckCreditLimitFromV1Request.builder()
                .docType(ReportConstants.HOUSE_BILL)
                .shipmentId(1L)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(checkCreditLimitFromV1Request);

        CheckCreditLimitFromV1Response mockCreditV1Response = new CheckCreditLimitFromV1Response();
        String errorResponse = "Shipment not exist for given id";
        // Mock
        when(shipmentDao.findById(1L)).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.checkCreditLimitFromV1(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorResponse, runnerResponse.getError().getMessage());
    }

    @Test
    void checkCreditLimitFromV1_invalid_doc_type() {
        CheckCreditLimitFromV1Request checkCreditLimitFromV1Request = CheckCreditLimitFromV1Request.builder()
                .docType("unsupported doc")
                .shipmentId(1L)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(checkCreditLimitFromV1Request);

        CheckCreditLimitFromV1Response mockCreditV1Response = new CheckCreditLimitFromV1Response();
        String errorResponse = "Please send a valid doc type for check credit limit.";

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.checkCreditLimitFromV1(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorResponse, runnerResponse.getError().getMessage());
    }

    @Test
    void checkCreditLimitFromV1_success() {
        CheckCreditLimitFromV1Request checkCreditLimitFromV1Request = CheckCreditLimitFromV1Request.builder()
                .docType(ReportConstants.HOUSE_BILL)
                .shipmentId(1L)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(checkCreditLimitFromV1Request);

        CheckCreditLimitFromV1Response mockCreditV1Response = new CheckCreditLimitFromV1Response();

        // Mock
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(v1ServiceUtil.validateCreditLimit(any(), any(), any(), any())).thenReturn(mockCreditV1Response);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.checkCreditLimitFromV1(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockCreditV1Response), httpResponse);
    }

    @Test
    void testCreateConsolidationNullCheck() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(false).build());
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        List<Containers> containers = new ArrayList<>();
        mockShipmentSettings();
        assertEquals(null, shipmentService.createConsolidation(shipmentDetails, containers));
    }

    @Test
    void testCreateConsolidationConsolidationLite() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());
        CarrierDetails carrierDetails = CarrierDetails.builder().build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().transportMode(Constants.TRANSPORT_MODE_SEA).carrierDetails(carrierDetails).build();
        List<Containers> containers = new ArrayList<>();
        String errorMessage = "Not able to create consolidation, before adding 'New Containers' , please provide ‘Origin’ and ‘Destination’ values.";
        mockShipmentSettings();
        Exception e = assertThrows(ValidationException.class, () -> shipmentService.createConsolidation(shipmentDetails, containers));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void testCreateConsolidationConsolidationLiteSameOriginDestination() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());
        CarrierDetails carrierDetails = CarrierDetails.builder().originPort("OriginPort").destinationPort("OriginPort").build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().transportMode(Constants.TRANSPORT_MODE_SEA).carrierDetails(carrierDetails).build();
        List<Containers> containers = new ArrayList<>();
        String errorMessage = "‘Origin’ and ‘Destination’ can't be same";
        mockShipmentSettings();
        Exception e = assertThrows(ValidationException.class, () -> shipmentService.createConsolidation(shipmentDetails, containers));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void testCreateConsolidation() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());

        PartyRequestV2 partyRequestV2 = new PartyRequestV2();
        partyRequestV2.setTenantId(1);

        Parties parties = Parties.builder().orgCode("1").build();

        doNothing().when(consolidationService).generateConsolidationNumber(any(ConsolidationDetails.class));
        CarrierDetails carrierDetails = CarrierDetails.builder().originPort("OriginPort").destinationPort("DestinationPort").build();
        Routings routings = new Routings();
        routings.setTenantId(1);
        routings.setMode("mode");

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .carrierDetails(carrierDetails)
                .direction(Constants.DIRECTION_IMP)
                .masterBill("1234")
                .routingsList(Arrays.asList(routings))
                .build();
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(carrierDetails);

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().carrierDetails(carrierDetails).sendingAgent(parties).receivingAgent(parties).build();
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), eq(false), anyBoolean())).thenReturn(consolidationDetails);
        mockShipmentSettings();
        commonUtils.getShipmentSettingFromContext().setEnableRouteMaster(true);
        ConsolidationDetails result = shipmentService.createConsolidation(shipmentDetails, new ArrayList<>());

        assertNotNull(result);
        assertEquals(carrierDetails, result.getCarrierDetails());
    }

    @Test
    void testCreateConsolidationDefaultDirectionExp() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());

        PartyRequestV2 partyRequestV2 = new PartyRequestV2();
        partyRequestV2.setTenantId(1);

        Parties parties = Parties.builder().orgCode("1").build();

        doNothing().when(consolidationService).generateConsolidationNumber(any(ConsolidationDetails.class));
        CarrierDetails carrierDetails = CarrierDetails.builder().originPort("OriginPort").destinationPort("DestinationPort").build();

        Routings routings = new Routings();
        routings.setTenantId(1);
        routings.setMode("mode");

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .carrierDetails(carrierDetails)
                .direction(Constants.DIRECTION_EXP)
                .masterBill("1234")
                .routingsList(Arrays.asList(routings))
                .build();

        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(carrierDetails);

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().carrierDetails(carrierDetails).shipmentType(Constants.DIRECTION_EXP).sendingAgent(parties).receivingAgent(parties).build();
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), eq(false), anyBoolean())).thenReturn(consolidationDetails);
        mockShipmentSettings();
        ConsolidationDetails result = shipmentService.createConsolidation(shipmentDetails, new ArrayList<>());

        assertNotNull(result);
        assertEquals(carrierDetails, result.getCarrierDetails());
    }


    @Test
    public void testExportExcel_NullRequest() throws IOException, IllegalAccessException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
        String errorMessage = "Shipment List Request is Null";
        Exception e = assertThrows(ValidationException.class, () -> shipmentService.exportExcel(httpServletResponse, commonRequestModel));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    public void testExportExcel() throws IOException, IllegalAccessException, ExecutionException, InterruptedException {

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        CarrierDetails carrierDetails = CarrierDetails.builder()
                .origin("origin_name")
                .originPort("originPort_name")
                .destination("destination_name")
                .destinationPort("destinationPort_name")
                .build();

        shipmentDetailsList.add(ShipmentDetails.builder().status(1).carrierDetails(carrierDetails).build());

        List<ShipmentExcelExportResponse> shipmentExcelExportResponseList = new ArrayList<>();
        CarrierDetailResponse carrierDetailResponse = CarrierDetailResponse.builder()
            .origin("origin_name")
            .originPort("originPort_name")
            .destination("destination_name")
            .destinationPort("destinationPort_name")
            .build();
        shipmentExcelExportResponseList.add(ShipmentExcelExportResponse.builder().status(1).carrierDetails(carrierDetailResponse).build());

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        HttpServletResponse response = mock(HttpServletResponse.class);
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ServletOutputStream servletOutputStream = new ServletOutputStream() {
            @Override
            public void write(int b) throws IOException {
                outputStream.write(b);
            }

            @Override
            public boolean isReady() {
                return true;
            }

            @Override
            public void setWriteListener(javax.servlet.WriteListener writeListener) {}
        };
        when(response.getOutputStream()).thenReturn(servletOutputStream);

        ListCommonRequest sampleRequest = constructListCommonRequest("id", 1, "=");
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(sampleRequest).build();
        ShipmentListResponse listResponse = ShipmentListResponse.builder().carrierDetails(carrierDetailResponse).status(1).build();
        when(jsonHelper.convertValue(any(), eq(ShipmentListResponse.class))).thenReturn(listResponse);
        shipmentService.exportExcel(response, commonRequestModel);

        verify(response, times(1)).setContentType(anyString());
        verify(response, times(1)).setHeader(anyString(), anyString());
        verify(response, times(1)).getOutputStream();
        assertNotNull(outputStream.toByteArray()); // Verify that the output stream contains data
    }

    @Test
    void completeV1ShipmentCreateAndUpdate() throws RunnerException {
        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).shipmentId("AIR-CAN-00001").shipmentCreatedOn(LocalDateTime.now()).consolidationList(new HashSet<>(Arrays.asList(consolidationDetails))).build();
        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);
        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);


        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(shipmentRequest, ShipmentDetailsResponse.class);
        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateBookingCarriageNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<BookingCarriageRequest> bookingCarriageRequestList = Arrays.asList(BookingCarriageRequest.builder().build());
        List<BookingCarriage> bookingCarriageList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).shipmentId("AIR-CAN-00001").shipmentCreatedOn(LocalDateTime.now()).consolidationList(new HashSet<>(Arrays.asList(consolidationDetails))).bookingCarriagesList(bookingCarriageRequestList).build();
        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(bookingCarriageDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(bookingCarriageDao.updateEntityFromShipment(bookingCarriageList, 1L, new ArrayList<>())).thenReturn(bookingCarriageList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateContainersListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        Set<ContainerRequest> containerRequests = new HashSet<>(Arrays.asList(ContainerRequest.builder().build()));
        List<Containers> containersList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails)))
                .containersList(containerRequests).build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateElDetailsNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<ELDetailsRequest> elDetailsRequests = Arrays.asList(ELDetailsRequest.builder().build());
        List<ELDetails> elDetailsList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).shipmentId("AIR-CAN-00001").shipmentCreatedOn(LocalDateTime.now()).consolidationList(new HashSet<>(Arrays.asList(consolidationDetails))).elDetailsList(elDetailsRequests).build();
        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(elDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(elDetailsDao.updateEntityFromShipment(elDetailsList, 1L, new ArrayList<>())).thenReturn(elDetailsList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateTruckDriverDetailsNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequests = Arrays.asList(TruckDriverDetailsRequest.builder().build());
        List<TruckDriverDetails> truckDriverDetails = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).shipmentId("AIR-CAN-00001").shipmentCreatedOn(LocalDateTime.now()).consolidationList(new HashSet<>(Arrays.asList(consolidationDetails))).truckDriverDetails(truckDriverDetailsRequests).build();
        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(truckDriverDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(truckDriverDetailsDao.updateEntityFromShipment(truckDriverDetails, 1L, new ArrayList<>())).thenReturn(truckDriverDetails);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateEventsRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<EventsRequest> eventsRequestList = Arrays.asList(EventsRequest.builder().build());
        List<Events> eventsList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L).shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails)))
                .eventsList(eventsRequestList)
                .build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(eventDao.updateEntityFromOtherEntity(eventsList, 1L, Constants.SHIPMENT, new ArrayList<>())).thenReturn(eventsList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateReferenceNumbersRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = Arrays.asList(ReferenceNumbersRequest.builder().build());
        List<ReferenceNumbers> referenceNumbersList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L).shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails)))
                .referenceNumbersList(referenceNumbersRequestList)
                .build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(referenceNumbersDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(referenceNumbersDao.updateEntityFromShipment(referenceNumbersList, 1L, new ArrayList<>())).thenReturn(referenceNumbersList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateRoutingsRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<RoutingsRequest> routingsRequestList = Arrays.asList(RoutingsRequest.builder().build());
        List<Routings> routingsList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails)))
                .routingsList(routingsRequestList).build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(routingsDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(routingsDao.updateEntityFromShipment(routingsList, 1L, new ArrayList<>())).thenReturn(routingsList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateServiceDetailsRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<ServiceDetailsRequest> serviceDetailsRequests = Arrays.asList(ServiceDetailsRequest.builder().build());
        List<ServiceDetails> serviceDetailsList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails)))
                .servicesList(serviceDetailsRequests)
                .build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(serviceDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(serviceDetailsDao.updateEntityFromShipment(serviceDetailsList, 1L, new ArrayList<>())).thenReturn(serviceDetailsList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateShipmentAddressesRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().build();
        List<PartiesRequest> partiesRequests = Arrays.asList(PartiesRequest.builder().build());
        List<Parties> partiesList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails)))
                .shipmentAddresses(partiesRequests).build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.empty();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(partiesDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(partiesDao.updateEntityFromOtherEntity(partiesList, 1L, Constants.SHIPMENT_ADDRESSES, new ArrayList<>())).thenReturn(partiesList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateNotesRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        List<NotesRequest> notesRequests = Arrays.asList(NotesRequest.builder().id(1L).build());
        List<Notes> notesList = new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails)))
                .notesList(notesRequests).build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consoleDetails));

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(notesDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(notesDao.updateEntityFromOtherEntity(notesList, 1L, Constants.SHIPMENT, new ArrayList<>())).thenReturn(notesList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), notesRequests, true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdateNonEmptyConsolidation() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        Set<ContainerRequest> containerRequests = new HashSet<>(Collections.singletonList(ContainerRequest.builder().build()));

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Collections.singletonList(consolidationDetails)))
                .containersList(containerRequests).build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        Optional<ConsolidationDetails> consoleDetails = Optional.of(ConsolidationDetails.builder().transportMode(Constants.TRANSPORT_MODE_AIR).build());

        PageImpl<Containers> containersPage = new PageImpl<>(Arrays.asList(Containers.builder().build()));
        when(containerDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(containersPage);

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(consoleDetails);

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeV1ShipmentCreateAndUpdatePackingRequestListNotNull() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        List<PackingRequest> packingRequestList = Arrays.asList(PackingRequest.builder().build());
        List<Packing> packingList= new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails)))
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .packingList(packingRequestList).build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consoleDetails));

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(packingDao.updateEntityFromShipment(anyList(), anyLong(), anyList(), any(), any(), any())).thenReturn(packingList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void partialUpdateIdNullTest() {
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();
        String errorMessage = "Request Id is null";
        Exception e = assertThrows(RunnerException.class, () -> shipmentService.partialUpdate(commonRequestModel, true));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void partialUpdateTestEntityNotPresent() {
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();
        String errorMessage = "Failed to fetch data for given constraint.";
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> shipmentService.partialUpdate(commonRequestModel, true));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void partialUpdateTest() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());

        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestShipmentIdNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().shipmentId(JsonNullable.of("AIR-CAN-00001")).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .build();

        doNothing().when(shipmentDetailsMapper).update(any(), any());

        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(shipmentDao.findByShipmentIdIn(any())).thenReturn(List.of(shipmentDetails));
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void partialUpdateTestShipmentIdNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).shipmentId(null).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();
        String errorMessage = "Failed to fetch data for given constraint.";
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> shipmentService.partialUpdate(commonRequestModel, true));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void partialUpdateTestMultipleShipments() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().shipmentId(JsonNullable.of("AIR-CAN-00001")).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .build();

        ArrayList<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(shipmentDetails);
        shipmentDetailsList.add(shipmentDetails);
        when(shipmentDao.findByShipmentIdIn(anyList())).thenReturn(shipmentDetailsList);
        String errorMessage = "Multiple entries found for unique field, data issue found.";
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> shipmentService.partialUpdate(commonRequestModel, true));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void partialUpdateTestAdditionalDetailsNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).additionalDetail(AdditionalDetailRequest.builder().build()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        LocalDateTime mockDateTime = LocalDateTime.now();
        AdditionalDetails additionalDetails = getmockAdditionalDetails(mockDateTime, true, true, true);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .additionalDetails(additionalDetails)
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        mockTenantSettings();
        mockShipmentSettings();
        when(additionalDetailDao.updateEntityFromShipment(any())).thenReturn(additionalDetails);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
        assertEquals(true, shipmentDetails.getAdditionalDetails().getDocTurnedOverToCustomer());

    }

    private AdditionalDetails getmockAdditionalDetails(LocalDateTime mockDateTime, Boolean isDocTurnedOverToCustomer,
                                                       Boolean isPickupByConsigneeCompleted,
                                                       Boolean isEmptyContainerReturned) {
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setShipmentId(1L);
        additionalDetails.setPickupDate(mockDateTime);
        additionalDetails.setCargoDeliveredDate(mockDateTime.plusDays(2));
        additionalDetails.setCustomReleaseDate(mockDateTime.plusDays(3));
        additionalDetails.setProofOfDeliveryDate(mockDateTime.plusDays(4));
        additionalDetails.setWarehouseCargoArrivalDate(mockDateTime.plusDays(5));
        additionalDetails.setDocTurnedOverToCustomer(isDocTurnedOverToCustomer);
        additionalDetails.setPickupByConsigneeCompleted(isPickupByConsigneeCompleted);
        additionalDetails.setEmptyContainerReturned(isEmptyContainerReturned);
        return additionalDetails;
    }

    @Test
    void partialUpdateTestCarrierDetailsNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        mockShipmentSettings();
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).carrierDetails(CarrierPatchRequest.builder().build()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .carrierDetails(CarrierDetails.builder().build())
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        doNothing().when(carrierDetailsMapper).update(any(), any());
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestTruckDriverDetailsNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).truckDriverDetails(Arrays.asList(TruckDriverDetailsRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        List<TruckDriverDetails> truckDriverDetailsList = Arrays.asList(TruckDriverDetails.builder().build());

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .truckDriverDetails(truckDriverDetailsList)
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(truckDriverDetailsDao.updateEntityFromShipment(any(), any())).thenReturn(truckDriverDetailsList);
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestPackingRequestNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).packingList(Arrays.asList(PackingRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        Packing packing = new Packing();
        packing.setShipmentId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .packingList(Arrays.asList(packing))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(packingDao.updateEntityFromShipment(any(), any(), any())).thenReturn(Arrays.asList(packing));
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestElDetailsNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).elDetailsList(Arrays.asList(ELDetailsRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .elDetailsList(Arrays.asList(ELDetails.builder().build()))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(elDetailsDao.updateEntityFromShipment(any(), any())).thenReturn(Arrays.asList(ELDetails.builder().build()));
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestEventRequestNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).eventsList(Arrays.asList(EventsRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        Events events = new Events();
        events.setId(1L);

        Events events2 = new Events();
        events2.setId(2L);
        events2.setContainerNumber("1234-abcd");
        events2.setLocationRole("abcd");

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .eventsList(Arrays.asList(events, events2))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(eventDao.updateEntityFromOtherEntity(any(), any(), any())).thenReturn(Arrays.asList(events, events2));
        doNothing().when(eventService).updateAtaAtdInShipment(any(), any(), any());
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
        assertEquals("1234-abcd", shipmentDetails.getEventsList().get(1).getContainerNumber());

    }

    @Test
    void partialUpdateTestNoteRequestNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).notesList(Arrays.asList(NotesRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .notesList(Arrays.asList(Notes.builder().build()))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(notesDao.updateEntityFromOtherEntity(any(), any(), any())).thenReturn(Arrays.asList(Notes.builder().build()));
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void partialUpdateTestReferenceNumberNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).referenceNumbersList(Arrays.asList(ReferenceNumbersRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        referenceNumbers.setShipmentId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .referenceNumbersList(Arrays.asList(referenceNumbers))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(referenceNumbersDao.updateEntityFromShipment(any(), any())).thenReturn(Arrays.asList(referenceNumbers));
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void partialUpdateTestRoutingsNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).routingsList(Arrays.asList(RoutingsRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        Routings routings = new Routings();
        routings.setTenantId(1);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .routingsList(Arrays.asList(routings))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(routingsDao.updateEntityFromShipment(any(), any())).thenReturn(List.of(routings));
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void partialUpdateTestServiceDetailNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder()
                .id(JsonNullable.of(1L))
                .servicesList(Arrays.asList(ServiceDetailsRequest.builder().build()))
                .build();

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setShipmentId(1L);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .servicesList(Arrays.asList(serviceDetails))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(serviceDetailsDao.updateEntityFromShipment(any(), any())).thenReturn(Arrays.asList(serviceDetails));
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void partialUpdateTestBookingCarriageNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).bookingCarriagesList(Arrays.asList(BookingCarriageRequest.builder().build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .bookingCarriagesList(Arrays.asList(BookingCarriage.builder().build()))
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        when(bookingCarriageDao.updateEntityFromShipment(any(), any())).thenReturn(Arrays.asList(BookingCarriage.builder().build()));
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void createShipmentInV2Test() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        PackingRequest packingRequest = PackingRequest.builder().build();
        packingRequest.setWeight(BigDecimal.valueOf(11.5));
        packingRequest.setVolume(BigDecimal.valueOf(11));
        packingRequest.setPacks("1");
        packingRequest.setLengthUnit("M");
        CustomerBookingRequest customerBookingRequest = CustomerBookingRequest.builder().id(1L).transportType(Constants.TRANSPORT_MODE_SEA).cargoType(Constants.CARGO_TYPE_FCL).carrierDetails(CarrierDetailRequest.builder().build()).orderManagementId("eaf227f3-de85-42b4-8180-cf48ccf568f9").build();
        customerBookingRequest.setPackingList(Collections.singletonList(packingRequest));
        when(notesDao.findByEntityIdAndEntityType(any(), any())).thenReturn(Arrays.asList(Notes.builder().build()));

        ShipmentOrder shipmentOrder = ShipmentOrder.builder().shipmentId(1L).orderGuid(UUID.fromString("eaf227f3-de85-42b4-8180-cf48ccf568f9")).build();
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        Parties importBroker = Parties.builder().orgCode("1223").build();
        PartiesRequest importBrokerRequest = PartiesRequest.builder().build();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setImportBroker(importBroker);
        additionalDetails.setExportBroker(importBroker);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").build().setReferenceNumbersList(Collections.singletonList(referenceNumbers)).setAdditionalDetails(additionalDetails).setGoodsDescription("Abcd");
        shipmentDetails.setGuid(UUID.randomUUID());
        shipmentDetails.setShipmentOrders(Collections.singletonList(shipmentOrder));
        shipmentDetails.setAdditionalDetails(new AdditionalDetails());
        shipmentDetails.setCarrierDetails(CarrierDetails.builder().build());

        when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(ConsolidationDetailsRequest.builder().build());
        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolRequest.class))).thenReturn(new AutoUpdateWtVolRequest());
        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolResponse.class))).thenReturn(new AutoUpdateWtVolResponse());
        doReturn(ResponseHelper.buildSuccessResponse(ConsolidationDetailsResponse.builder().build())).when(consolidationService).createFromBooking(any());

        ReferenceNumbersRequest referenceNumberObj2 = ReferenceNumbersRequest.builder().build();

        when(jsonHelper.convertValue(anyList(), any(TypeReference.class))).thenReturn(Collections.singletonList(referenceNumberObj2));

        when(jsonHelper.convertValueToList(any(), eq(Packing.class))).thenReturn(Collections.singletonList(new Packing()));
        when(packingDao.saveEntityFromShipment(any(), any())).thenReturn(Collections.singletonList(new Packing()));

        when(jsonHelper.convertValueToList(any(), eq(ReferenceNumbers.class))).thenReturn(Collections.singletonList(referenceNumbers));
        when(referenceNumbersDao.saveEntityFromShipment(any(), any())).thenReturn(Collections.singletonList(referenceNumbers));

        when(jsonHelper.convertValueToList(any(), eq(ShipmentOrder.class))).thenReturn(Collections.singletonList(shipmentOrder));
        when(shipmentOrderDao.updateEntityFromShipment(any(), any())).thenReturn(Collections.singletonList(shipmentOrder));

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails);
        when(notesDao.findByEntityIdAndEntityType(any(), any())).thenReturn(null);

        ShipmentDetailsResponse shipmentDetailsResponse = jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(shipmentDetailsResponse);

        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(new CarrierDetails());
        when(routingsDao.generateDefaultRouting(any(), any())).thenReturn(List.of());

        when(jsonHelper.convertValueToList(any(), eq(ConsolidationDetails.class))).thenReturn(List.of(new ConsolidationDetails()));
        mockShipmentSettings();
        mockTenantSettings();
        commonUtils.getShipmentSettingFromContext().setEnableRouteMaster(true);

        when(orderManagementAdapter.getOrderByGuid(any())).thenReturn(shipmentDetails);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.createShipmentInV2(customerBookingRequest);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void assignAllContainerTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).build());
        ContainerAssignListRequest containerAssignListRequest = new ContainerAssignListRequest();
        containerAssignListRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(containerAssignListRequest).build();
        PageImpl<Containers> containersPage = new PageImpl<>(Arrays.asList(Containers.builder()
                .allocatedWeight(BigDecimal.TEN)
                .achievedWeight(BigDecimal.TEN)
                .allocatedVolume(BigDecimal.TEN)
                .achievedWeightUnit(Constants.WEIGHT_UNIT_KG)
                .achievedVolumeUnit(Constants.VOLUME_UNIT_M3)
                .allocatedWeightUnit(Constants.WEIGHT_UNIT_KG)
                .allocatedVolumeUnit(Constants.VOLUME_UNIT_M3)
                .build()));

        when(containerDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(containersPage);
        when(shipmentsContainersMappingDao.findByContainerId(any())).thenReturn(Arrays.asList(ShipmentsContainersMapping.builder().shipmentId(1L).build()));
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().transportMode(Constants.TRANSPORT_MODE_SEA).shipmentType(Constants.CARGO_TYPE_FCL).build();
        shipmentDetails1.setGuid(UUID.randomUUID());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails1));
        //doNothing().when(shipmentsContainersMappingDao).assignContainers(any(), any());
        mockShipmentSettings();

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.assignAllContainers(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testAssignAllContainers_CatchBlock() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).build());
        CommonRequestModel commonRequestModel = Mockito.mock(CommonRequestModel.class);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.assignAllContainers(commonRequestModel);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void calculateAutoUpdateWeightVolumeInShipmentTest() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder()
                .weightChargeableUnit(Constants.WEIGHT_UNIT_KG)
                .volumeChargeableUnit(Constants.VOLUME_UNIT_M3)
                .build());

        AutoUpdateWtVolRequest autoUpdateWtVolRequest = new AutoUpdateWtVolRequest();
        autoUpdateWtVolRequest.setPackingList(Arrays.asList(PackingRequest.builder().build()));

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(autoUpdateWtVolRequest).build();
        AutoUpdateWtVolResponse autoUpdateWtVolResponse = new AutoUpdateWtVolResponse();

        List<Packing> packingList = new ArrayList<>();

        when(jsonHelper.convertValue(autoUpdateWtVolRequest, AutoUpdateWtVolResponse.class)).thenReturn(autoUpdateWtVolResponse);
        when(jsonHelper.convertValueToList(anyList(), eq(Packing.class))).thenReturn(packingList);
        when(packingService.calculatePackSummary(any(), any(), any(), any())).thenReturn(new PackSummaryResponse());

        assertNotNull(shipmentService.calculateAutoUpdateWtVolInShipment(commonRequestModel));
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.calculateAutoUpdateWtVolInShipment(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(autoUpdateWtVolResponse), httpResponse);
    }

    @Test
    void testcalculateAutoUpdateWtVolInShipment_CatchBlock() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).build());
        CommonRequestModel commonRequestModel = Mockito.mock(CommonRequestModel.class);
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.calculateAutoUpdateWtVolInShipment(commonRequestModel);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetDataMappings_CatchBlock() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).build());
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.getMasterDataMappings();
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void assignShipmentContainersTestCatchBlock() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).build());
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.assignShipmentContainers(CommonRequestModel.builder().build());
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByIdTest() {
        var shipId = 1L;
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(shipId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setId(shipId);
        shipmentDetails.setGuid(UUID.randomUUID());

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(notesDao.findByEntityIdAndEntityType(anyLong(), eq(Constants.CUSTOMER_BOOKING))).thenReturn(Arrays.asList(Notes.builder().entityId(1L).build()));
        when(notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(anyList(), anyString())).thenReturn(Map.of(1L, 5));

        when(jsonHelper.convertValueToList(anyList(), eq(NotesResponse.class))).thenReturn(Arrays.asList(NotesResponse.builder().build()));
        when(modelMapper.map(any(), any())).thenReturn(ShipmentDetailsResponse.builder().build());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.retrieveById(commonRequestModel, true);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void completeUpdateConsolidationListNotEmpty_success() throws RunnerException {
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setId(1L);

        ConsolidationDetails consolidationDetails2 = new ConsolidationDetails();
        consolidationDetails2.setId(2L);

        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails1)));
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);

        when(shipmentDao.findById(any()))
                .thenReturn(
                        Optional.of(
                                shipmentDetails
                                        .setTransportMode(Constants.TRANSPORT_MODE_AIR)
                                        .setSourceTenantId(1L)
                                        .setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails2)))
                                        .setContainersList(new HashSet<>())));

        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);

        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());

        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails1));
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeUpdateConsolidationListNotEmpty_success_Air() throws RunnerException {
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setId(1L);

        ConsolidationDetails consolidationDetails2 = new ConsolidationDetails();
        consolidationDetails2.setId(2L);

        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR)
                .setJobType(Constants.SHIPMENT_TYPE_DRT)
                .setSourceTenantId(1L)
                .setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails2)))
                .setContainersList(new HashSet<>());
        ShipmentDetails oldEntity = jsonTestUtility.getTestShipment()
                .setTransportMode(Constants.TRANSPORT_MODE_AIR)
                .setSourceTenantId(1L)
                .setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails2)))
                .setContainersList(new HashSet<>());
        oldEntity.getCarrierDetails().setShippingLine("ABC AirLine");
        oldEntity.setId(1L);
        mockShipment.setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails1)));
        mockShipment.getAdditionalDetails().setSci(AwbConstants.T1);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).iataTactFlag(true).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        Awb awb = new Awb().setAwbGoodsDescriptionInfo(List.of(new AwbGoodsDescriptionInfo()));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(oldEntity);

        when(shipmentDao.findById(any())).thenReturn(Optional.of(oldEntity));

        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(awbDao.findByShipmentId(anyLong())).thenReturn(List.of(awb));

        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());

        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails1));
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeUpdateConsolidationListNotEmpty_success_Air_SciUpdate() throws RunnerException {
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setId(1L);
        consolidationDetails1.setSci(AwbConstants.T1);

        ConsolidationDetails consolidationDetails2 = new ConsolidationDetails();
        consolidationDetails2.setId(2L);

        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR)
                .setJobType(Constants.SHIPMENT_TYPE_DRT)
                .setSourceTenantId(1L)
                .setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails2)))
                .setContainersList(new HashSet<>());
        ShipmentDetails oldEntity = jsonTestUtility.getTestShipment()
                .setTransportMode(Constants.TRANSPORT_MODE_AIR)
                .setSourceTenantId(1L)
                .setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails2)))
                .setContainersList(new HashSet<>());
        oldEntity.getCarrierDetails().setShippingLine("ABC AirLine");
        oldEntity.setId(1L);
        mockShipment.setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails1)));
        mockShipment.getAdditionalDetails().setSci("T2");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).iataTactFlag(true).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        Awb awb = new Awb().setAwbGoodsDescriptionInfo(List.of(new AwbGoodsDescriptionInfo()));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(oldEntity);

        when(shipmentDao.findById(any())).thenReturn(Optional.of(oldEntity));

        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(awbDao.findByShipmentId(anyLong())).thenReturn(List.of(awb));

        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());

        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails1));
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeUpdateConsolidationListNotEmpty_success_Air_() throws RunnerException {
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setId(1L);
        TenantSettingsDetailsContext.getCurrentTenantSettings().setIsMAWBColoadingEnabled(true);

        ConsolidationDetails consolidationDetails2 = new ConsolidationDetails();
        consolidationDetails2.setId(2L);

        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR)
                .setDirection(Constants.DIRECTION_EXP)
                .setJobType(Constants.SHIPMENT_TYPE_DRT)
                .setSourceTenantId(1L)
                .setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails2)))
                .setContainersList(new HashSet<>())
                .setContainsHazardous(true);
        ShipmentDetails oldEntity = jsonTestUtility.getTestShipment()
                .setDirection(Constants.DIRECTION_EXP)
                .setTransportMode(Constants.TRANSPORT_MODE_AIR)
                .setSourceTenantId(1L)
                .setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails2)))
                .setContainersList(new HashSet<>())
                .setContainsHazardous(true);
        oldEntity.getCarrierDetails().setShippingLine("ABC AirLine");
        oldEntity.setId(1L);
        mockShipment.setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails1)));
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).iataTactFlag(true).airDGFlag(true).build());
        UserContext.getUser().getPermissions().put(PermissionConstants.airDG, true);

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        Awb awb = new Awb().setAwbGoodsDescriptionInfo(List.of(new AwbGoodsDescriptionInfo()));

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(oldEntity);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(oldEntity));

        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(awbDao.findByShipmentId(anyLong())).thenReturn(List.of(awb));

        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails1));
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }


    @Test
    void completeUpdateConsolidationListNotEmpty_success_Air_NewConsoleAdded() throws RunnerException {
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setId(1L);
        TenantSettingsDetailsContext.getCurrentTenantSettings().setIsMAWBColoadingEnabled(true);

        ConsolidationDetails consolidationDetails2 = new ConsolidationDetails();
        consolidationDetails2.setId(2L);
        consolidationDetails2.setCarrierDetails(new CarrierDetails());
        consolidationDetails2.setTransportMode(Constants.TRANSPORT_MODE_AIR).setShipmentType(Constants.DIRECTION_EXP);

        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR)
                .setDirection(Constants.DIRECTION_EXP)
                .setJobType(Constants.SHIPMENT_TYPE_DRT)
                .setSourceTenantId(1L)
                .setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails2)))
                .setContainersList(new HashSet<>())
                .setContainsHazardous(true);
        ShipmentDetails oldEntity = jsonTestUtility.getTestShipment()
                .setDirection(Constants.DIRECTION_EXP)
                .setTransportMode(Constants.TRANSPORT_MODE_AIR)
                .setConsolidationList(Set.of())
                .setSourceTenantId(1L)
                .setContainersList(new HashSet<>())
                .setContainsHazardous(true);
        oldEntity.getCarrierDetails().setShippingLine("ABC AirLine");
        oldEntity.setId(1L);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).iataTactFlag(true).airDGFlag(true).build());
        UserContext.getUser().getPermissions().put(PermissionConstants.airDG, true);

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        Awb awb = new Awb().setAwbGoodsDescriptionInfo(List.of(new AwbGoodsDescriptionInfo()));

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(oldEntity);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(oldEntity));

        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(awbDao.findByShipmentId(anyLong())).thenReturn(List.of(awb));

//        when(containerDao.findByConsolidationId(any())).thenReturn(Arrays.asList(Containers.builder().build()));
//        when(awbDao.findByConsolidationId(any())).thenReturn(Arrays.asList(Awb.builder().build()));

        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails1));
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @ParameterizedTest
    @ValueSource(booleans = {
            true, false
    })
    void completeUpdateConsolidationListNotEmpty_success_isDGUser(boolean dgUser) throws RunnerException {
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setId(1L);

        ConsolidationDetails consolidationDetails2 = new ConsolidationDetails();
        consolidationDetails2.setId(2L);

        shipmentDetails.setId(1L);
        shipmentDetails.setContainsHazardous(true);
        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails1)));
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).airDGFlag(true).build());
        Map<String, Boolean> permissions = new HashMap<>();
        permissions.put(PermissionConstants.airDG, dgUser);
        UserContext.getUser().setPermissions(permissions);

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.findById(any()))
                .thenReturn(
                        Optional.of(
                                shipmentDetails
                                        .setTransportMode(Constants.TRANSPORT_MODE_AIR)
                                        .setSourceTenantId(1L)
                                        .setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails2)))
                                        .setContainersList(new HashSet<>())));

        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        mockShipmentSettings();
        if(dgUser) {
            when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
            when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);

            when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
            when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
            when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails1));
            mockTenantSettings();
            ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);
            assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
        } else {
            assertThrows(ValidationException.class, () -> shipmentService.completeUpdate(commonRequestModel));
        }
        UserContext.getUser().setPermissions(new HashMap<>());
    }

    @Test
    void completeUpdateConsolidationListNotEmpty_error_AirDG() throws RunnerException {
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setId(1L);

        ConsolidationDetails consolidationDetails2 = new ConsolidationDetails();
        consolidationDetails2.setId(2L);

        shipmentDetails.setId(1L);
        shipmentDetails.setContainsHazardous(true);
        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails1)));
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).airDGFlag(true).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        when(shipmentDao.findById(any()))
                .thenReturn(
                        Optional.of(
                                shipmentDetails
                                        .setTransportMode(Constants.TRANSPORT_MODE_AIR)
                                        .setSourceTenantId(1L)
                                        .setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails2)))
                                        .setContainersList(new HashSet<>())));

        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        assertThrows(ValidationException.class, () -> shipmentService.completeUpdate(commonRequestModel));
    }

    @Test
    void completeUpdateConsolidationListNotEmpty_error_AirDG_False() throws RunnerException {
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setHazardous(true);
        consolidationDetails1.setId(1L);

        ConsolidationDetails consolidationDetails2 = new ConsolidationDetails();
        consolidationDetails2.setId(2L);

        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails1)));
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).airDGFlag(true).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        when(shipmentDao.findById(any()))
                .thenReturn(
                        Optional.of(
                                shipmentDetails
                                        .setTransportMode(Constants.TRANSPORT_MODE_AIR)
                                        .setSourceTenantId(1L)
                                        .setConsolidationList(new HashSet<>(Arrays.asList(consolidationDetails2)))
                                        .setContainersList(new HashSet<>())));

        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        assertThrows(ValidationException.class, () -> shipmentService.completeUpdate(commonRequestModel));
    }

    @Test
    void retrieveByIdTestCatchBlock() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).build());
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.retrieveById(CommonRequestModel.builder().build());
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void completeUpdateContainerIds_success() throws RunnerException {
        shipmentDetails.setId(1L);
        shipmentDetails.setConsolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())));
        ShipmentDetails mockShipment = shipmentDetails;
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).shipConsolidationContainerEnabled(true).build());

        Routings routings = new Routings();
        routings.setMode(Constants.TRANSPORT_MODE_SEA);

        RoutingsRequest routingsRequest = new RoutingsRequest();
        routingsRequest.setMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        mockShipmentRequest.setBookingCarriagesList(Arrays.asList(BookingCarriageRequest.builder().build()));
        mockShipmentRequest.setTruckDriverDetails(Arrays.asList(TruckDriverDetailsRequest.builder().build()));
        mockShipmentRequest.setReplaceConsoleRoute(true);
        mockShipmentRequest.setConsolidationList(new HashSet<>(Arrays.asList(ConsolidationDetailsRequest.builder().build())));
        mockShipmentRequest.setRoutingsList(Arrays.asList(routingsRequest));
        mockShipmentRequest.setCreateMainLegRoute(true);

        ContainerIdDltReq containerIdDltReq = new ContainerIdDltReq();
        containerIdDltReq.setId(1L);

        mockShipmentRequest.setDeletedContainerIds(Arrays.asList(containerIdDltReq));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        PartyRequestV2 partyRequestV2 = new PartyRequestV2();
        partyRequestV2.setTenantId(1);

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.findById(any()))
                .thenReturn(
                        Optional.of(
                                shipmentDetails
                                        .setContainersList(new HashSet<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(routings);

        Containers containers = new Containers();
        containers.setGuid(UUID.randomUUID());

        Packing packing = new Packing();
        packing.setId(1L);
        List<Packing> packingList = new ArrayList<>();
        packingList.add(packing);

        mockShipmentSettings();
        mockTenantSettings();
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void createTestShipmentSuccess() throws RunnerException {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails);
        List<ShipmentDetails> shipmentDetailsList = shipmentService.createTestShipment(1);
        assertNotNull(shipmentDetailsList);
    }

    @Test
    void testExportExcelUnlocationNotNull() throws IOException, IllegalAccessException, ExecutionException, InterruptedException {

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        CarrierDetails carrierDetails = CarrierDetails.builder()
                .origin("origin_name")
                .originPort("originPort_name")
                .destination("destination_name")
                .destinationPort("destinationPort_name")
                .build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().status(1).carrierDetails(carrierDetails).build();
        shipmentDetailsList.add(shipmentDetails);

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        List<IRunnerResponse> responseList = convertEntityListToDtoList(shipmentDetailsList);
        ShipmentListResponse shipmentListResponse = (ShipmentListResponse) responseList.get(0);
        shipmentListResponse.getCarrierDetails().setUnlocationData(new HashMap<>());

        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                responseList,
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );

        HttpServletResponse response = mock(HttpServletResponse.class);
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ServletOutputStream servletOutputStream = new ServletOutputStream() {
            @Override
            public void write(int b) throws IOException {
                outputStream.write(b);
            }

            @Override
            public boolean isReady() {
                return true;
            }

            @Override
            public void setWriteListener(javax.servlet.WriteListener writeListener) {}
        };
        when(response.getOutputStream()).thenReturn(servletOutputStream);

        ListCommonRequest sampleRequest = constructListCommonRequest("id", 1, "=");
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(sampleRequest).build();
        CarrierDetailResponse carrierDetailResponse = CarrierDetailResponse.builder()
            .origin("origin_name")
            .originPort("originPort_name")
            .destination("destination_name")
            .destinationPort("destinationPort_name")
            .build();
        ShipmentListResponse listResponse = ShipmentListResponse.builder().carrierDetails(carrierDetailResponse).status(1).build();
        when(jsonHelper.convertValue(any(), eq(ShipmentListResponse.class))).thenReturn(listResponse);
        shipmentService.exportExcel(response, commonRequestModel);

        verify(response, times(1)).setContentType(anyString());
        verify(response, times(1)).setHeader(anyString(), anyString());
        verify(response, times(1)).getOutputStream();
        assertNotNull(outputStream.toByteArray()); // Verify that the output stream contains data
    }

    @Test
    void testFullShipmentListCatch() throws IOException, IllegalAccessException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fullShipmentsList(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void testFullShipmentList() throws IOException, IllegalAccessException {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(ShipmentDetails.builder().build());

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        when(modelMapper.map(any(), eq(ShipmentDetailsResponse.class))).thenReturn(new ShipmentDetailsResponse());

        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToFullShipmentList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fullShipmentsList(commonRequestModel);
        assertEquals(expectedResponse, httpResponse);
    }

    @Test
    void testFullShipmentExternalList() {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setIncludeColumns(List.of("abc", "def"));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(ShipmentDetails.builder().build());

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
          when(commonUtils.getShipmentDetailsResponse(any(), anyList())).thenReturn(ShipmentDetailsResponse.builder().build());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fullShipmentsExternalList(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testFullShipmentExternalListWithNullRequest() {
        CommonRequestModel commonRequestModel = mock(CommonRequestModel.class);
        when(commonRequestModel.getData()).thenReturn(null);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fullShipmentsExternalList(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }


    @Test
    void testFullShipmentExternalListWithNullIncludeColumn() {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(ShipmentDetails.builder().build());

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fullShipmentsExternalList(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void testFullShipmentExternalListWithEmptyIncludeColumn() {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setIncludeColumns(List.of());
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(ShipmentDetails.builder().build());

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fullShipmentsExternalList(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void assignShipmentContainers() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).build());
        ShipmentContainerAssignRequest shipmentContainerAssignRequest = new ShipmentContainerAssignRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentContainerAssignRequest).build();

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setShipmentId("SHP001");

        Containers container = Containers.builder().bookingId(1L).isPart(true).shipmentsList(new HashSet<>(Arrays.asList(shipmentDetails))).containerNumber("1").build();
        PageImpl<Containers> containersPage = new PageImpl<>(Arrays.asList(container));
        when(containerDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(containersPage);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.assignShipmentContainers(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void assignShipmentContainersIsFcl() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).build());
        ShipmentContainerAssignRequest shipmentContainerAssignRequest = new ShipmentContainerAssignRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentContainerAssignRequest).build();

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setShipmentId("SHP001");
        shipmentDetails.setGuid(UUID.randomUUID());

        Containers container = Containers.builder().bookingId(1L).isPart(true).containerNumber("1").build();
        PageImpl<Containers> containersPage = new PageImpl<>(Arrays.asList(container));
        when(containerDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(containersPage);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));

        when(containerDao.saveAll(anyList())).thenReturn(Arrays.asList(Containers.builder().build()));
        doNothing().when(shipmentsContainersMappingDao).assignContainers(any(), any(), any());
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.assignShipmentContainers(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void listCatch() {
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.list(CommonRequestModel.builder().build());
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listRequestNull() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.list(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }
    
    @Test
    void list() {
        Criteria criteria = Criteria.builder().fieldName("wayBillNumber").value(1).build();
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(Arrays.asList(FilterCriteria.builder().criteria(criteria).innerFilter(Arrays.asList(FilterCriteria.builder().build())).build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        GuidsListResponse guidsListResponse = new GuidsListResponse();
        when(v1Service.fetchWayBillNumberFilterGuids(any())).thenReturn(guidsListResponse);

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.list(commonRequestModel);
        assertEquals(expectedResponse, httpResponse);
    }

    @Test
    void listShipmentsWithNotifications() {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(new ArrayList<>()).build();
        listCommonRequest.setNotificationFlag(true);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        PageImpl<Long> shipmentIdPage = new PageImpl<>(List.of(1L));
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        when(shipmentDao.getIdWithPendingActions(eq(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED), any())).thenReturn(shipmentIdPage);

        var expectedResponse = ResponseHelper.buildListSuccessResponse(
            convertEntityListToDtoList(shipmentDetailsPage.getContent()),
            shipmentDetailsPage.getTotalPages(),
            shipmentDetailsPage.getTotalElements()
        );

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.list(commonRequestModel);
        assertEquals(expectedResponse, httpResponse);
    }

    @Test
    void listShipmentsWithOrderCounts() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setShipmentOrders(Collections.singletonList(ShipmentOrder.builder().build()));
        Criteria criteria = Criteria.builder().fieldName("wayBillNumber").value(1).build();
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(Arrays.asList(FilterCriteria.builder().criteria(criteria).innerFilter(Arrays.asList(FilterCriteria.builder().build())).build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        GuidsListResponse guidsListResponse = new GuidsListResponse();
        when(v1Service.fetchWayBillNumberFilterGuids(any())).thenReturn(guidsListResponse);

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(shipmentDetails);
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );

        ResponseEntity<?> httpResponse = shipmentService.list(commonRequestModel);
        assertEquals(expectedResponse, httpResponse);
        var data  = (ShipmentListResponse) (((RunnerListResponse<?>) Objects.requireNonNull(httpResponse.getBody())).getData()).get(0);
        assertEquals(1, data.getOrdersCount());
    }

    @Test
    void deleteCatch() {
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.delete(CommonRequestModel.builder().build());
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void deleteRequestNull() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.delete(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void deleteRequestIdNull() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(null).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.delete(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void deleteRequestShipmentNull() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.delete(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void delete() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(ShipmentDetails.builder().build()));

        when(jsonHelper.convertToJson(any())).thenReturn("OldEntity");
        doNothing().when(shipmentDao).delete(any());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.delete(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void partialUpdateTestSameOrg() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .jobType(Constants.SHIPMENT_TYPE_DRT)
                .consignee(Parties.builder().orgCode("org").build())
                .consigner(Parties.builder().orgCode("org").build())
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());

        String errorMessage = "Consignor & Consignee parties can't be selected as same.";
        Exception e = assertThrows(RunnerException.class, () -> shipmentService.partialUpdate(commonRequestModel, true));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void partialUpdateTestRoa() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .jobType(Constants.SHIPMENT_TYPE_DRT)
                .transportMode(Constants.TRANSPORT_MODE_ROA)
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());

        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void partialUpdateTestSea() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .jobType(Constants.SHIPMENT_TYPE_DRT)
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());

        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void checkRaStatusFieldsTest() {
        HashMap<String, Object> hm = new HashMap<>();
        hm.put("RegulatedAgent", true);

        HashMap<String, Map<String, Object>> map = new HashMap();
        map.put("org1#add1", hm);

        AdditionalDetails additionalDetails = new AdditionalDetails();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().additionalDetails(additionalDetails).build();
        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().addresses(map).build();
        Parties parties = Parties.builder().orgCode("org1").addressCode("add1").build();
        assertFalse(shipmentService.checkRaStatusFields(shipmentDetails, orgAddressResponse, parties));
    }

    @Test
    void checkRaStatusFieldsTestFalse() {
        HashMap<String, Object> hm = new HashMap<>();
        hm.put("KnownConsignor", true);

        HashMap<String, Map<String, Object>> map = new HashMap();
        map.put("org1#add1", hm);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().addresses(map).build();
        Parties parties = Parties.builder().orgCode("org2").addressCode("add2").build();
        assertTrue(shipmentService.checkRaStatusFields(shipmentDetails, orgAddressResponse, parties));
    }

    @Test
    void checkRaStatusFieldsTest_ScreeningStatus_Empty() {
        HashMap<String, Object> hm = new HashMap<>();
        hm.put("RegulatedAgent", true);

        HashMap<String, Map<String, Object>> map = new HashMap();
        map.put("org1#add1", hm);

        AdditionalDetails additionalDetails = new AdditionalDetails();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().additionalDetails(additionalDetails).build();
        additionalDetails.setScreeningStatus(List.of());
        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().addresses(map).build();
        Parties parties = Parties.builder().orgCode("org1").addressCode("add1").build();
        assertFalse(shipmentService.checkRaStatusFields(shipmentDetails, orgAddressResponse, parties));
    }

    @Test
    void checkRaStatusFieldsTest_ScreeningStatus() {
        HashMap<String, Object> hm = new HashMap<>();
        hm.put("RegulatedAgent", true);

        HashMap<String, Map<String, Object>> map = new HashMap();
        map.put("org1#add1", hm);

        AdditionalDetails additionalDetails = new AdditionalDetails();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().additionalDetails(additionalDetails).build();
        additionalDetails.setScreeningStatus(List.of("Screening"));
        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().addresses(map).build();
        Parties parties = Parties.builder().orgCode("org1").addressCode("add1").build();
        assertFalse(shipmentService.checkRaStatusFields(shipmentDetails, orgAddressResponse, parties));
    }

    @Test
    void checkRaStatusFieldsTest_SecurityStatus() {
        HashMap<String, Object> hm = new HashMap<>();
        hm.put("RegulatedAgent", true);

        HashMap<String, Map<String, Object>> map = new HashMap();
        map.put("org1#add1", hm);

        AdditionalDetails additionalDetails = new AdditionalDetails();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().additionalDetails(additionalDetails).securityStatus("Security").build();
        additionalDetails.setScreeningStatus(List.of("Screening"));
        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().addresses(map).build();
        Parties parties = Parties.builder().orgCode("org1").addressCode("add1").build();
        assertTrue(shipmentService.checkRaStatusFields(shipmentDetails, orgAddressResponse, parties));
    }

    @Test
    void retrieveByIdAsycNullRequest() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
        CompletableFuture<ResponseEntity<IRunnerResponse>> httpResponse = shipmentService.retrieveByIdAsync(commonRequestModel);
        httpResponse.whenComplete((responseEntity, throwable) -> {
            assertTrue(throwable instanceof CompletionException);
            assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
        });
    }

    @Test
    void retrieveByIdAsycNullRequestIdNull() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        CompletableFuture<ResponseEntity<IRunnerResponse>> httpResponse = shipmentService.retrieveByIdAsync(commonRequestModel);
        httpResponse.whenComplete((responseEntity, throwable) -> {
            assertTrue(throwable instanceof CompletionException);
            assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
        });
    }

    @Test
    void retrieveByIdAsycTest() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(notesDao.findByEntityIdAndEntityType(anyLong(), eq(Constants.CUSTOMER_BOOKING))).thenReturn(Arrays.asList(Notes.builder().entityId(1L).build()));

        when(jsonHelper.convertValueToList(anyList(), eq(NotesResponse.class))).thenReturn(Arrays.asList(NotesResponse.builder().build()));
        when(modelMapper.map(any(), any())).thenReturn(ShipmentDetailsResponse.builder().build());

        CompletableFuture<ResponseEntity<IRunnerResponse>> httpResponse = shipmentService.retrieveByIdAsync(commonRequestModel);
        httpResponse.whenComplete((responseEntity, throwable) -> {
            assertTrue(throwable instanceof CompletionException);
            assertEquals(ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().customerBookingNotesList(Arrays.asList(NotesResponse.builder().build())).build()), responseEntity);
        });
    }

    @Test
    void listAsycCatch() {
        CompletableFuture<ResponseEntity<IRunnerResponse>> httpResponse = shipmentService.listAsync(CommonRequestModel.builder().build());
        assertNotNull(httpResponse);
        httpResponse.whenComplete((responseEntity, throwable) -> {
            assertTrue(throwable instanceof CompletionException);
            assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
        });
    }

    @Test
    void listAsycRequestNull() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
        CompletableFuture<ResponseEntity<IRunnerResponse>> httpResponse = shipmentService.listAsync(commonRequestModel);
        httpResponse.whenComplete((responseEntity, throwable) -> {
            assertTrue(throwable instanceof CompletionException);
            assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
        });
    }

    @Test
    void listAsyc() {
        Criteria criteria = Criteria.builder().fieldName("wayBillNumber").value(1).build();
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(Arrays.asList(FilterCriteria.builder().criteria(criteria).innerFilter(Arrays.asList(FilterCriteria.builder().build())).build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );

        CompletableFuture<ResponseEntity<IRunnerResponse>> httpResponse = shipmentService.listAsync(commonRequestModel);
        httpResponse.whenComplete((responseEntity, throwable) -> {
            assertTrue(throwable instanceof CompletionException);
            assertEquals(expectedResponse, responseEntity);
        });
    }

    @Test
    void completeUpdateAutoCreateConsole() throws RunnerException {
        shipmentDetails.setId(1L);
        shipmentDetails.setConsolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())));
        shipmentDetails.setContainersList(new HashSet<>(Arrays.asList(Containers.builder().build())));
        ShipmentDetails mockShipment = shipmentDetails;
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).shipConsolidationContainerEnabled(true).build());

        Routings routings = new Routings();
        routings.setMode(Constants.TRANSPORT_MODE_SEA);

        RoutingsRequest routingsRequest = new RoutingsRequest();
        routingsRequest.setMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        mockShipmentRequest.setBookingCarriagesList(Arrays.asList(BookingCarriageRequest.builder().build()));
        mockShipmentRequest.setTruckDriverDetails(Arrays.asList(TruckDriverDetailsRequest.builder().build()));
        mockShipmentRequest.setReplaceConsoleRoute(true);
        mockShipmentRequest.setConsolidationList(new HashSet<>(Arrays.asList(ConsolidationDetailsRequest.builder().build())));
        mockShipmentRequest.setRoutingsList(Arrays.asList(routingsRequest));
        mockShipmentRequest.setCreateMainLegRoute(true);

        ContainerIdDltReq containerIdDltReq = new ContainerIdDltReq();
        containerIdDltReq.setId(1L);

        mockShipmentRequest.setDeletedContainerIds(Arrays.asList(containerIdDltReq));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        PartyRequestV2 partyRequestV2 = new PartyRequestV2();
        partyRequestV2.setTenantId(1);

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.findById(any()))
                .thenReturn(
                        Optional.of(
                                shipmentDetails));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(routings);
        when(commonUtils.convertToEntityList(any(), any(), any())).thenReturn(Arrays.asList(Containers.builder().build()));
        when(commonUtils.convertToEntityList(any(), eq(Events.class), any())).thenReturn(Arrays.asList(Events.builder().build()));
        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), eq(false))).thenReturn(Arrays.asList(Containers.builder().build()));


        when(hblService.checkAllContainerAssigned(any(), any(), any())).thenReturn(Hbl.builder().build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());
        PartyRequestV2 partyRequestV3 = new PartyRequestV2();
        partyRequestV3.setTenantId(1);

        Parties parties = Parties.builder().orgCode("1").build();

        doNothing().when(consolidationService).generateConsolidationNumber(any(ConsolidationDetails.class));
        CarrierDetails carrierDetails = CarrierDetails.builder().originPort("OriginPort").destinationPort("DestinationPort").build();
        Routings routings1 = new Routings();
        routings1.setTenantId(1);
        routings1.setMode("mode");

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .carrierDetails(carrierDetails)
                .direction(Constants.DIRECTION_IMP)
                .masterBill("1234")
                .routingsList(Arrays.asList(routings1))
                .build();
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(carrierDetails);

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().carrierDetails(carrierDetails).sendingAgent(parties).receivingAgent(parties).build();
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), eq(false), anyBoolean())).thenReturn(consolidationDetails);

        Containers containers = new Containers();
        containers.setGuid(UUID.randomUUID());
        Packing packing = new Packing();
        packing.setId(1L);
        List<Packing> packingList = new ArrayList<>();
        packingList.add(packing);
        mockShipmentSettings();
        mockTenantSettings();
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void create_successContainerListNotEmpty() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());
        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setShipmentId("AIR-CAN-00001");
        mockShipment.setContainersList(new HashSet<>(Arrays.asList(Containers.builder().consolidationId(1L).build())));
        mockShipment.setElDetailsList(Arrays.asList(ELDetails.builder().build()));

        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        mockShipment.setReferenceNumbersList(Arrays.asList(referenceNumbers));

        ServiceDetails serviceDetails = new ServiceDetails();
        mockShipment.setServicesList(Arrays.asList(serviceDetails));

        mockShipment.setNotesList(Arrays.asList(Notes.builder().build()));

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);

        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);
        when(commonUtils.convertToEntityList(any(), any(), any())).thenReturn(Arrays.asList(Containers.builder().build()));
        when(commonUtils.convertToEntityList(any(), eq(Events.class), any())).thenReturn(Arrays.asList(Events.builder().build()));

        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), eq(false))).thenReturn(Arrays.asList(Containers.builder().build()));
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(consolidationDetailsDao.save(any(), eq(false), anyBoolean())).thenReturn(ConsolidationDetails.builder().build());

        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(ConsolidationDetails.builder().interBranchConsole(false).build()));
        mockShipmentSettings();
        mockTenantSettings();

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.create(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void syncShipmentAuditLogsToServiceTestCatch() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.syncShipmentAuditLogsToService(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void syncShipmentAuditLogsToServiceTest() {
        AuditLogsSyncRequest auditLogsSyncRequest = new AuditLogsSyncRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(auditLogsSyncRequest).build();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.syncShipmentAuditLogsToService(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void syncShipmentAuditLogsToServiceIdNotPresentNullTest() {
        AuditLogsSyncRequest auditLogsSyncRequest = new AuditLogsSyncRequest();
        auditLogsSyncRequest.setGuid(UUID.randomUUID());
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(auditLogsSyncRequest).build();
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.syncShipmentAuditLogsToService(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void syncShipmentAuditLogsToServiceIdNotNullTest() {
        AuditLogsSyncRequest auditLogsSyncRequest = new AuditLogsSyncRequest();
        auditLogsSyncRequest.setGuid(UUID.randomUUID());
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(auditLogsSyncRequest).build();
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(ShipmentDetails.builder().build()));
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.syncShipmentAuditLogsToService(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void createCatch() throws RunnerException {
        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setShipmentId("AIR-CAN-00001");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDao.save(any(), eq(false))).thenThrow(RunnerException.class);
        mockShipmentSettings();
        assertThrows(ValidationException.class, () -> {
            shipmentService.create(commonRequestModel);
        });
    }

    @Test
    void createParties() {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        PartiesRequest partiesRequest = PartiesRequest.builder().build();
        when(mockObjectMapper.convertValue(any(), eq(Packing.class))).thenReturn(new Packing());
        shipmentService.createParties(shipmentDetails, partiesRequest);
        verify(packingDao).save(any(Packing.class));
    }

    @Test
    void retrieveByIdOrGuidCatchTest() {
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().build();
        Exception e = assertThrows(RunnerException.class, () -> {
            shipmentService.retrieveByIdOrGuid(shipmentRequest);
        });

        String errorMessage ="Either Id or Guid is required";
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void retrieveByIdOrGuidEntityNotPresentTest() {
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().build();
        shipmentRequest.setGuid(UUID.randomUUID());
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.empty());
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentService.retrieveByIdOrGuid(shipmentRequest);
        });

        String errorMessage ="Failed to fetch data for given constraint.";
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void retrieveByIdOrGuidTest() throws RunnerException {
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().build();
        shipmentRequest.setGuid(UUID.randomUUID());
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(ShipmentDetails.builder().build()));
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        ShipmentDetails response = shipmentService.retrieveByIdOrGuid(shipmentRequest).get();
        assertEquals(shipmentDetails, response);
    }

    @Test
    void calculateWtVolInShipmentOnChangesCatch() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.calculateWtVolInShipmentOnChanges(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void calculateWtVolInShipmentOnChanges() throws RunnerException {
        AutoUpdateWtVolRequest request = new AutoUpdateWtVolRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        AutoUpdateWtVolResponse response = new AutoUpdateWtVolResponse();
        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolResponse.class))).thenReturn(response);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.calculateWtVolInShipmentOnChanges(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(response), httpResponse);
    }

    @Test
    void updateCatch() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        assertThrows(RunnerException.class, () -> {
            shipmentService.update(commonRequestModel);
        });
    }

    @Test
    void update() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().containersList(new HashSet<>(Arrays.asList(Containers.builder().build()))).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(ShipmentRequest.builder().id(1L).build()).build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);

        doNothing().when(eventService).updateAtaAtdInShipment(any(), any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);

        ContainerResponse containerResponse = new ContainerResponse();
        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().containersList(new HashSet<>(Arrays.asList(containerResponse))).build();
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(shipmentDetailsResponse);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.update(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(shipmentDetailsResponse), httpResponse);
    }

    @Test
    void updateWithGuid() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().containersList(new HashSet<>(Arrays.asList(Containers.builder().build()))).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(ShipmentRequest.builder().id(1L).build()).build();

        ShipmentDetails savedShipmentDetails = new ShipmentDetails();
        savedShipmentDetails.setGuid(UUID.randomUUID());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(savedShipmentDetails));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);

        shipmentDetails.setGuid(UUID.randomUUID());
        assertThrows(RunnerException.class, () -> {shipmentService.update(commonRequestModel);});
    }

    @Test
    void updateWithContainerListNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().containersList(new HashSet<>(Arrays.asList(Containers.builder().build()))).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(ShipmentRequest.builder().id(1L).build()).build();

        ShipmentDetails savedShipmentDetails = new ShipmentDetails();
        savedShipmentDetails.setGuid(UUID.randomUUID());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(savedShipmentDetails));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(savedShipmentDetails);

        shipmentDetails.setGuid(savedShipmentDetails.getGuid());
        doNothing().when(eventService).updateAtaAtdInShipment(any(), any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);

        ContainerResponse containerResponse = new ContainerResponse();
        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().containersList(new HashSet<>(Arrays.asList(containerResponse))).build();
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(shipmentDetailsResponse);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.update(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(shipmentDetailsResponse), httpResponse);
    }

    @Test
    void fetchEmailsShipmentIdConsolidationIdNull() {
        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentService.fetchEmails(null, null);
        });
    }

    @Test
    void fetchEmailsTestShipmentIdNotNull() {
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentService.fetchEmails(1L, null);
        });
    }

    @Test
    void fetchEmailsTestConsolidationIdNotNull() {
        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentService.fetchEmails(null, 1L);
        });
    }

    @Test
    void showAssignAllContainersCatch() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        assertEquals(HttpStatus.BAD_REQUEST, shipmentService.showAssignAllContainers(commonRequestModel).getStatusCode());
    }

    @Test
    void autoGenerateEventsCompletedStatusTest() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).carrierDetails(CarrierPatchRequest.builder().build()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .carrierDetails(CarrierDetails.builder().build())
                .build();

        ShipmentDetails shipmentDetailsResponse = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .carrierDetails(CarrierDetails.builder().build())
                .status(ShipmentStatus.Completed.getValue())
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetailsResponse);
        doNothing().when(carrierDetailsMapper).update(any(), any());
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void autoGenerateEventsTest() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).carrierDetails(CarrierPatchRequest.builder().build()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .carrierDetails(CarrierDetails.builder().build())
                .build();

        ShipmentDetails shipmentDetailsResponse = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .carrierDetails(CarrierDetails.builder().build())
                .status(ShipmentStatus.Confirmed.getValue())
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetailsResponse);
        doNothing().when(carrierDetailsMapper).update(any(), any());
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void getShipment() throws RunnerException {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();

        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails);
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(ShipmentSettingsDetails.builder().customisedSequence(true).build()));
        when(productEngine.IdentifyProduct(eq(shipmentDetails), anyList())).thenReturn(null);

        Page<ShipmentDetails> page = new PageImpl<>(Collections.singletonList(shipmentDetails), Pageable.unpaged(), 0L); // Create a mock Page
        when(shipmentDao.findAll(any(), any())).thenReturn(page);
        when(v1Service.getShipmentSerialNumber()).thenReturn("123");

        ShipmentDetails httpResponse = shipmentService.getShipment(shipmentDetails);
        assertEquals(shipmentDetails, httpResponse);
    }

    @Test
    void getShipmentIdentifierNotNull() throws RunnerException {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();

        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails);
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(ShipmentSettingsDetails.builder().customisedSequence(true).build()));

        when(productEngine.IdentifyProduct(eq(shipmentDetails), anyList())).thenReturn(new TenantProducts());
        when(getNextNumberHelper.getProductSequence(any(), any())).thenReturn(new ProductSequenceConfig());

        Page<ShipmentDetails> page = new PageImpl<>(Collections.singletonList(shipmentDetails), Pageable.unpaged(), 0L); // Create a mock Page
        when(shipmentDao.findAll(any(), any())).thenReturn(page);
        when(v1Service.getShipmentSerialNumber()).thenReturn("123");

        ShipmentDetails httpResponse = shipmentService.getShipment(shipmentDetails);
        assertEquals(shipmentDetails, httpResponse);
    }

    @Test
    void cloneShipmentCatchRequestNull() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        assertEquals(HttpStatus.BAD_REQUEST, shipmentService.cloneShipment(commonRequestModel).getStatusCode());
    }

    @Test
    void cloneShipmentCatchIdNull() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(CommonGetRequest.builder().build()).build();
        assertEquals(HttpStatus.BAD_REQUEST, shipmentService.cloneShipment(commonRequestModel).getStatusCode());
    }

    @Test
    void cloneShipmentNotPresent() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(CommonGetRequest.builder().id(1L).build()).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        assertEquals(HttpStatus.BAD_REQUEST, shipmentService.cloneShipment(commonRequestModel).getStatusCode());
    }

    @Test
    void getShipmentFromConsolNP1() {

        Long consolidationId = 1L;
        String bookingNumber = "bookingNumber";
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().restrictHblGen(true).build());
        Parties parties = new Parties();
        parties.setType("NP1");
        testConsol.setConsolidationAddresses(Arrays.asList(parties));
        ConsolidationDetailsResponse consolidationDetailsResponse = objectMapper.convertValue(testConsol, ConsolidationDetailsResponse.class);
        ConsolidationListResponse consolidationListResponse = objectMapper.convertValue(testConsol, ConsolidationListResponse.class);


        // Mock
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(testConsol));
        when(modelMapper.map(any(), eq(ConsolidationDetailsResponse.class))).thenReturn(consolidationDetailsResponse);
        when(modelMapper.map(any(), eq(ConsolidationListResponse.class))).thenReturn(consolidationListResponse);
        when(jsonHelper.convertValue(any(), eq(PartiesResponse.class))).thenReturn(new PartiesResponse());
        //when(modelMapper.map(any(), eq(RoutingsResponse.class))).thenReturn(new RoutingsResponse());
        mockShipmentSettings();
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getShipmentFromConsol(consolidationId, bookingNumber);
        SpringContext.setApplicationContext(applicationContext);
        Mockito.when(applicationContext.getBean(CommonUtils.class)).thenReturn(commonUtils);
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        ShipmentDetailsResponse shipmentResponse = objectMapper.convertValue(runnerResponse.getData(), ShipmentDetailsResponse.class);
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(Constants.SYSTEM, shipmentResponse.getSource());
        assertEquals(Constants.SHIPMENT_TYPE_STD, shipmentResponse.getJobType());
        assertEquals(bookingNumber, shipmentResponse.getBookingNumber());
    }

    @Test
    void partialUpdateTestAirMessaging() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().EnableAirMessaging(true).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).additionalDetail(AdditionalDetailRequest.builder().build()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setShipmentId(1L);
        additionalDetails.setImportBroker(Parties.builder().addressCode("code").build());
        additionalDetails.setExportBroker(Parties.builder().addressCode("code").build());

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .additionalDetails(additionalDetails)
                .consigner(Parties.builder().orgCode("org1").addressCode("add1").build())
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);

        HashMap<String, Object> hm = new HashMap<>();
        hm.put("RegulatedAgent", true);

        HashMap<String, Map<String, Object>> map = new HashMap();
        map.put("org1#add1", hm);


        when(additionalDetailDao.updateEntityFromShipment(any())).thenReturn(additionalDetails);
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void createShipmentPayloadAutoUpdateContainerTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).build());
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Containers containers = Containers.builder().build();
        containers.setId(1L);
        PackingResponse packing = new PackingResponse();
        packing.setContainerId(1L);
        ContainerResponse containerResponse = new ContainerResponse();
        containerResponse.setId(1L);
        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder()
                .containersList(new HashSet<>(Arrays.asList(containerResponse)))
                .packingList(Arrays.asList(packing))
                .truckDriverDetails(Arrays.asList(TruckDriverDetailsResponse.builder().containerId(1L).build()))
                .containerAutoWeightVolumeUpdate(true)
                .build();

        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        shipmentService.createShipmentPayload(shipmentDetails, shipmentDetailsResponse, true);
        verify(masterDataUtils, atLeastOnce()).withMdc(any());
    }

    @Test
    void createShipmentPayloadTest() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Containers containers = Containers.builder().build();
        containers.setId(1L);
        PackingResponse packing = new PackingResponse();
        packing.setContainerId(1L);
        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder()
                .containersList(new HashSet<>(List.of()))
                .packingList(Arrays.asList(packing))
                .truckDriverDetails(Arrays.asList(TruckDriverDetailsResponse.builder().containerId(1L).build()))
                .build();

        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        shipmentService.createShipmentPayload(shipmentDetails, shipmentDetailsResponse, true);
    }

    @Test
    void fetchAllMasterDataByKey() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Containers containers = Containers.builder().build();
        containers.setId(1L);
        PackingResponse packing = new PackingResponse();
        packing.setContainerId(1L);
        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder()
                .containersList(new HashSet<>(List.of()))
                .packingList(Arrays.asList(packing))
                .truckDriverDetails(Arrays.asList(TruckDriverDetailsResponse.builder().containerId(1L).build()))
                .build();

        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        shipmentService.fetchAllMasterDataByKey(shipmentDetails, shipmentDetailsResponse);
        verify(masterDataUtils, times(12)).withMdc(any());
    }

    @Test
    void completeUpdateCalculateAutoContainerWeightAndVolume() throws RunnerException {
        shipmentDetails.setId(1L);
        shipmentDetails.setConsolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())));
        shipmentDetails.setContainerAutoWeightVolumeUpdate(true);
        shipmentDetails.setOceanDGStatus(OceanDGStatus.OCEAN_DG_ACCEPTED);

        Packing packing = new Packing();
        packing.setContainerId(1L);
        packing.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        packing.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        packing.setHazardous(true);
        packing.setDGClass("1.1");
        shipmentDetails.setPackingList(Arrays.asList(packing));

        Routings routings = new Routings();
        routings.setMode(Constants.TRANSPORT_MODE_SEA);
        routings.setIsSelectedForDocument(true);
        routings.setId(1L);
        shipmentDetails.setRoutingsList(Arrays.asList(routings));

        Events events = new Events();
        events.setId(1L);
        events.setContainerNumber("abcd-efgh-1234-ijkl");
        events.setLocationRole("bcdfh");
        shipmentDetails.setEventsList(List.of(events));

        ShipmentDetails mockShipment = shipmentDetails;
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).shipConsolidationContainerEnabled(true).build());


        RoutingsRequest routingsRequest = new RoutingsRequest();
        routingsRequest.setMode(Constants.TRANSPORT_MODE_SEA);
        routingsRequest.setIsSelectedForDocument(true);
        routingsRequest.setId(1L);

        EventsRequest eventsRequest = new EventsRequest();
        eventsRequest.setContainerNumber("abcd-efgh-1234-ijkl");
        eventsRequest.setLocationRole("bcdfh");
        eventsRequest.setId(1L);

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        mockShipmentRequest.setBookingCarriagesList(Arrays.asList(BookingCarriageRequest.builder().build()));
        mockShipmentRequest.setTruckDriverDetails(Arrays.asList(TruckDriverDetailsRequest.builder().build()));
        mockShipmentRequest.setReplaceConsoleRoute(true);
        mockShipmentRequest.setConsolidationList(new HashSet<>(Arrays.asList(ConsolidationDetailsRequest.builder().build())));
        mockShipmentRequest.setRoutingsList(Arrays.asList(routingsRequest));
        mockShipmentRequest.setEventsList(List.of(eventsRequest));
        mockShipmentRequest.setCreateMainLegRoute(true);

        ContainerIdDltReq containerIdDltReq = new ContainerIdDltReq();
        containerIdDltReq.setId(1L);

        mockShipmentRequest.setDeletedContainerIds(Arrays.asList(containerIdDltReq));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        PartyRequestV2 partyRequestV2 = new PartyRequestV2();
        partyRequestV2.setTenantId(1);

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.findById(any()))
                .thenReturn(
                        Optional.of(
                                shipmentDetails
                                        .setContainersList(new HashSet<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(commonUtils.checkIfDGClass1(any())).thenReturn(true);
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(ConsolidationDetails.builder().build()));
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(routings);
        when(jsonHelper.convertValueToList(any(), eq(ContainerRequest.class))).thenReturn(Arrays.asList(ContainerRequest.builder()
                .id(1L)
                .dgClass("1.1")
                .unNumber("unNum")
                .properShippingName("psn")
                .build()));

        Containers containers = new Containers();
        containers.setGuid(UUID.randomUUID());

        Packing packing2 = new Packing();
        packing2.setId(1L);
        List<Packing> packingList = new ArrayList<>();
        packingList.add(packing2);

        mockShipmentSettings();
        mockTenantSettings();
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
        assertEquals(true, mockShipmentResponse.getRoutingsList().get(0).getIsSelectedForDocument());
        assertEquals("bcdfh", mockShipmentResponse.getEventsList().get(0).getLocationRole());

    }

    @Test
    void completeUpdateCalculateAutoContainerWeightAndVolume_dg() throws RunnerException {
        shipmentDetails.setId(1L);
        shipmentDetails.setConsolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())));
        shipmentDetails.setContainerAutoWeightVolumeUpdate(true);
        shipmentDetails.setOceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED);

        Packing packing = new Packing();
        packing.setId(3L);
        packing.setContainerId(1L);
        packing.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        packing.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        packing.setHazardous(true);
        packing.setDGClass("1.1");
        shipmentDetails.setPackingList(Arrays.asList(packing));

        Routings routings = new Routings();
        routings.setMode(Constants.TRANSPORT_MODE_SEA);
        routings.setIsSelectedForDocument(true);
        routings.setId(1L);
        shipmentDetails.setRoutingsList(Arrays.asList(routings));

        Events events = new Events();
        events.setId(1L);
        events.setContainerNumber("abcd-efgh-1234-ijkl");
        events.setLocationRole("bcdfh");
        shipmentDetails.setEventsList(List.of(events));
        Containers containers_ = new Containers();
        shipmentDetails.setContainersList(Set.of(containers_));

        ShipmentDetails mockShipment = shipmentDetails;
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).shipConsolidationContainerEnabled(true).build());


        RoutingsRequest routingsRequest = new RoutingsRequest();
        routingsRequest.setMode(Constants.TRANSPORT_MODE_SEA);
        routingsRequest.setIsSelectedForDocument(true);
        routingsRequest.setId(1L);

        EventsRequest eventsRequest = new EventsRequest();
        eventsRequest.setContainerNumber("abcd-efgh-1234-ijkl");
        eventsRequest.setLocationRole("bcdfh");
        eventsRequest.setId(1L);

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        mockShipmentRequest.setBookingCarriagesList(Arrays.asList(BookingCarriageRequest.builder().build()));
        mockShipmentRequest.setTruckDriverDetails(Arrays.asList(TruckDriverDetailsRequest.builder().build()));
        mockShipmentRequest.setReplaceConsoleRoute(true);
        mockShipmentRequest.setConsolidationList(new HashSet<>(Arrays.asList(ConsolidationDetailsRequest.builder().build())));
        mockShipmentRequest.setRoutingsList(Arrays.asList(routingsRequest));
        mockShipmentRequest.setEventsList(List.of(eventsRequest));
        mockShipmentRequest.setCreateMainLegRoute(true);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        PartyRequestV2 partyRequestV2 = new PartyRequestV2();
        partyRequestV2.setTenantId(1);

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.findById(any()))
                .thenReturn(
                        Optional.of(
                                shipmentDetails
                                        .setContainersList(new HashSet<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
        when(jsonHelper.convertValueToList(any(), eq(ConsoleShipmentMapping.class))).thenReturn(List.of(ConsoleShipmentMapping.builder().build()));
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(routings);

        Containers containers = new Containers();
        containers.setGuid(UUID.randomUUID());

        PageImpl<Containers> containersPage = new PageImpl<>(Arrays.asList(containers));
//        when(containerDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(containersPage);

        Packing packing2 = new Packing();
        packing2.setId(1L);
        List<Packing> packingList = new ArrayList<>();
        packingList.add(packing2);

        PageImpl<Packing> packingPage = new PageImpl<>(Arrays.asList(packing2));
//        when(packingDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(packingPage);
        mockShipmentSettings();
        mockTenantSettings();
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
        assertEquals(true, mockShipmentResponse.getRoutingsList().get(0).getIsSelectedForDocument());
        assertEquals("bcdfh", mockShipmentResponse.getEventsList().get(0).getLocationRole());

    }

    @Test
    void createFromBookingCatch() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        assertThrows(ValidationException.class, () -> {
            shipmentService.createFromBooking(commonRequestModel);
        });
    }

    @Test
    void createFromBookingAutoGenerateEvent() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(true).build());
        ShipmentRequest shipmentRequest = new ShipmentRequest();
        shipmentRequest.setShipmentId("SHP001");
        shipmentRequest.setGuid(UUID.randomUUID());
        shipmentRequest.setContainersList(new HashSet<>(Arrays.asList(ContainerRequest.builder().build())));
        shipmentRequest.setPackingList(Arrays.asList(PackingRequest.builder().build()));
        shipmentRequest.setRoutingsList(Arrays.asList(RoutingsRequest.builder().build()));
        shipmentRequest.setNotesList(Arrays.asList(NotesRequest.builder().build()));
        shipmentRequest.setAdditionalDetails(new AdditionalDetailRequest());
        shipmentRequest.setCarrierDetails(CarrierDetailRequest.builder().build());

        Packing packing = new Packing();
        Routings routings = new Routings();

        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentRequest).build();

        when(jsonHelper.convertValueToList(any(), eq(Packing.class))).thenReturn(Arrays.asList(packing));

        when(jsonHelper.convertValueToList(any(), eq(Containers.class))).thenReturn(Arrays.asList(Containers.builder().build()));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(shipmentDetails);
        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails);

        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(shipmentRequest, ShipmentDetailsResponse.class);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(shipmentDetailsResponse);
        when(hblService.checkAllContainerAssigned(any(), any(), any())).thenReturn(Hbl.builder().build());

        when(jsonHelper.convertValue(any(), eq(Notes.class))).thenReturn(Notes.builder().build());
        when(notesDao.save(any())).thenReturn(Notes.builder().build());
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.createFromBooking(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(shipmentDetailsResponse), httpResponse);
    }

    @Test
    void calculateWtVol() throws RunnerException {
        AutoUpdateWtVolRequest request = new AutoUpdateWtVolRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();

        AutoUpdateWtVolResponse response = new AutoUpdateWtVolResponse();
        response.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        response.setVolumeUnit(Constants.VOLUME_UNIT_M3);

        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolResponse.class))).thenReturn(response);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(new VolumeWeightChargeable());
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.calculateWtVolInShipmentOnChanges(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(response), httpResponse);
    }

    @Test
    void completeUpdateCatch() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        assertThrows(NullPointerException.class, () -> {
            shipmentService.completeUpdate(commonRequestModel);
        });
    }

    @Test
    void completeRetrieveByIdCatch() throws ExecutionException, InterruptedException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        assertEquals(HttpStatus.BAD_REQUEST, shipmentService.completeRetrieveById(commonRequestModel).getStatusCode());
    }

    @Test
    void completeRetrieveByIdNullRequest() throws ExecutionException, InterruptedException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        assertEquals(HttpStatus.BAD_REQUEST, shipmentService.completeRetrieveById(commonRequestModel).getStatusCode());
    }

    @Test
    void completeRetrieveById() throws ExecutionException, InterruptedException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeRetrieveById(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void fetchShipmentsForContainer() throws RunnerException {
        Long consoleId = 1L;
        CommonGetRequest request = CommonGetRequest.builder().id(consoleId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        List<ConsoleShipmentMapping> consoleShipmentMappings = new ArrayList<>();
        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setShipmentId(1L);
        mapping.setConsolidationId(1L);
        consoleShipmentMappings.add(mapping);
        shipmentDetails.setEventsList(null);

        Set<Containers> containersList = new HashSet<>();
        containersList.add(Containers.builder().containerCode(Constants.Cont20).containerNumber("CON123").build());
        containersList.add(Containers.builder().containerCode(Constants.Cont40).build());
        containersList.add(Containers.builder().containerCode(Constants.Cont20GP).build());
        containersList.add(Containers.builder().containerCode(Constants.Cont20RE).build());
        containersList.add(Containers.builder().containerCode(Constants.Cont40GP).build());
        containersList.add(Containers.builder().containerCode(Constants.Cont40RE).build());

        shipmentDetails.setContainersList(containersList);
        when(consoleShipmentMappingDao.findByConsolidationId(consoleId)).thenReturn(consoleShipmentMappings);
        when(consoleShipmentMappingDao.pendingStateCountBasedOnShipmentId(anyList(), anyInt())).thenReturn(Map.of(2L, 5));
        when(notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(anyList(), anyString())).thenReturn(Map.of(2L, 5));

        shipmentDetails.setId(1L);
        List<ShipmentDetails> shipments = List.of(shipmentDetails);
        List<IRunnerResponse> shipmentResponse = convertEntityListToDtoList(shipments);
        PageImpl<ShipmentDetails> page = new PageImpl<>(List.of(shipmentDetails));

        ResponseEntity<IRunnerResponse> expectedResponse = ResponseHelper.buildListSuccessResponse(
                shipmentResponse,
                page.getTotalPages(),
                page.getTotalElements()
        );

        when(shipmentDao.findAll(any(), any())).thenReturn(page);
        ResponseEntity<IRunnerResponse> result = shipmentService.fetchShipmentsForConsoleId(commonRequestModel);
        assertEquals(expectedResponse, result);
    }

    @Test
    void calculateAutoUpdateWeightVolumeInShipmentLevelContainersTest() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder()
                .weightChargeableUnit(Constants.WEIGHT_UNIT_KG)
                .volumeChargeableUnit(Constants.VOLUME_UNIT_M3)
                .isShipmentLevelContainer(true)
                .build());

        AutoUpdateWtVolRequest autoUpdateWtVolRequest = new AutoUpdateWtVolRequest();
        autoUpdateWtVolRequest.setPackingList(Arrays.asList(PackingRequest.builder().build()));
        autoUpdateWtVolRequest.setContainersList(new ArrayList<>(Arrays.asList(ContainerRequest.builder().build())));
        autoUpdateWtVolRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(autoUpdateWtVolRequest).build();
        AutoUpdateWtVolResponse autoUpdateWtVolResponse = new AutoUpdateWtVolResponse();

        List<Packing> packingList = new ArrayList<>();
        List<Containers> containersList = new ArrayList<>();
        containersList.add(Containers.builder().build());

        when(jsonHelper.convertValue(autoUpdateWtVolRequest, AutoUpdateWtVolResponse.class)).thenReturn(autoUpdateWtVolResponse);
        when(jsonHelper.convertValueToList(anyList(), eq(Packing.class))).thenReturn(packingList);
        when(jsonHelper.convertValueToList(anyList(), eq(Containers.class))).thenReturn(containersList);

        VolumeWeightChargeable volumeWeightChargeable = new VolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(BigDecimal.ONE);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        mockShipmentSettings();
        mockTenantSettings();
        assertNotNull(shipmentService.calculateAutoUpdateWtVolInShipment(commonRequestModel));
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.calculateAutoUpdateWtVolInShipment(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(autoUpdateWtVolResponse), httpResponse);
    }

    @Test
    void calculateAutoUpdateWeightVolumeInShipmentContainersTest() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder()
                .weightChargeableUnit(Constants.WEIGHT_UNIT_KG)
                .volumeChargeableUnit(Constants.VOLUME_UNIT_M3)
                .isShipmentLevelContainer(true)
                .build());

        AutoUpdateWtVolRequest autoUpdateWtVolRequest = new AutoUpdateWtVolRequest();
        autoUpdateWtVolRequest.setPackingList(Arrays.asList(PackingRequest.builder().build()));
        autoUpdateWtVolRequest.setContainersList(new ArrayList<>(Arrays.asList(ContainerRequest.builder().build())));
        autoUpdateWtVolRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(autoUpdateWtVolRequest).build();
        AutoUpdateWtVolResponse autoUpdateWtVolResponse = new AutoUpdateWtVolResponse();

        List<Packing> packingList = new ArrayList<>();
        List<Containers> containersList = new ArrayList<>();
        containersList.add(Containers.builder().build());

        when(jsonHelper.convertValue(autoUpdateWtVolRequest, AutoUpdateWtVolResponse.class)).thenReturn(autoUpdateWtVolResponse);
        when(jsonHelper.convertValueToList(anyList(), eq(Packing.class))).thenReturn(packingList);
        when(jsonHelper.convertValueToList(anyList(), eq(Containers.class))).thenReturn(containersList);

        VolumeWeightChargeable volumeWeightChargeable = new VolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(BigDecimal.ONE);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        mockShipmentSettings();
        mockTenantSettings();
        assertNotNull(shipmentService.calculateAutoUpdateWtVolInShipment(commonRequestModel));
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.calculateAutoUpdateWtVolInShipment(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(autoUpdateWtVolResponse), httpResponse);
    }


    @Test
    void calculateAutoUpdateWeightVolumeInShipmentP100est() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(true).build());

        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder()
                .weightChargeableUnit(Constants.WEIGHT_UNIT_KG)
                .volumeChargeableUnit(Constants.VOLUME_UNIT_M3)
                .isShipmentLevelContainer(true)
                .build());

        AutoUpdateWtVolRequest autoUpdateWtVolRequest = new AutoUpdateWtVolRequest();
        autoUpdateWtVolRequest.setPackingList(Arrays.asList(PackingRequest.builder().build()));
        autoUpdateWtVolRequest.setContainersList(new ArrayList<>(Arrays.asList(ContainerRequest.builder().build())));
        autoUpdateWtVolRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(autoUpdateWtVolRequest).build();
        AutoUpdateWtVolResponse autoUpdateWtVolResponse = new AutoUpdateWtVolResponse();

        List<Packing> packingList = new ArrayList<>();
        List<Containers> containersList = new ArrayList<>();
        containersList.add(Containers.builder().build());

        when(jsonHelper.convertValue(autoUpdateWtVolRequest, AutoUpdateWtVolResponse.class)).thenReturn(autoUpdateWtVolResponse);
        when(jsonHelper.convertValueToList(anyList(), eq(Packing.class))).thenReturn(packingList);
        when(jsonHelper.convertValueToList(anyList(), eq(Containers.class))).thenReturn(containersList);

        VolumeWeightChargeable volumeWeightChargeable = new VolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(BigDecimal.ONE);
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(volumeWeightChargeable);
        mockShipmentSettings();
        mockTenantSettings();
        assertNotNull(shipmentService.calculateAutoUpdateWtVolInShipment(commonRequestModel));
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.calculateAutoUpdateWtVolInShipment(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(autoUpdateWtVolResponse), httpResponse);
    }

    @Test
    void getAllMasterDataTestCatch() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        assertEquals(HttpStatus.BAD_REQUEST, shipmentService.getAllMasterData(commonRequestModel).getStatusCode());
    }

    @Test
    void getAllMasterDataEmptyShipmentTest() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().id(1L).build();
        when(shipmentDao.findShipmentByIdWithQuery(any())).thenReturn(Optional.empty());
        assertEquals(HttpStatus.BAD_REQUEST, shipmentService.getAllMasterData(commonRequestModel).getStatusCode());
    }

    @Test
    void getAllMasterDataTest() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().id(1L).build();
        when(shipmentDao.findShipmentByIdWithQuery(any())).thenReturn(Optional.of(ShipmentDetails.builder().build()));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(ShipmentDetailsResponse.builder().build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        Map<String, Object> response = new HashMap<>();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.getAllMasterData(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(response), httpResponse);
    }

    @Test
    void getIdFromGuidCatch() {
        assertEquals(HttpStatus.BAD_REQUEST, shipmentService.getIdFromGuid(null).getStatusCode());
    }

    @Test
    void getGuidFromIdCatch() {
        assertEquals(HttpStatus.BAD_REQUEST, shipmentService.getGuidFromId(null).getStatusCode());
    }

    @Test
    void attachListShipmentEvents() {
        long consolidationId = 1L;
        AttachListShipmentRequest attachListShipmentRequest = new AttachListShipmentRequest();
        attachListShipmentRequest.setConsolidationId(consolidationId);
        attachListShipmentRequest.setEtdMatch(false); // Set other properties as needed
        attachListShipmentRequest.setEtaMatch(true);
        attachListShipmentRequest.setScheduleMatch(true);
        attachListShipmentRequest.setFilterCriteria(new ArrayList<>());

        List<Events> eventsList = new ArrayList<>();

        Events event1 = new Events();
        event1.setEventCode(EventConstants.INVGNTD);

        Events event2 = new Events();
        event2.setEventCode(EventConstants.TAXSG);

        Events event3 = new Events();
        event3.setEventCode(EventConstants.CSEDI);

        Events event4 = new Events();
        event4.setEventCode(EventConstants.AMSEDI);

        eventsList.add(event1);
        eventsList.add(event2);
        eventsList.add(event3);
        eventsList.add(event4);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(attachListShipmentRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setCarrierDetails(CarrierDetails.builder().eta(LocalDateTime.now()).etd(LocalDateTime.now()).shippingLine(Constants.SHIPPING_LINE).flightNumber(Constants.FLIGHT_NUMBER).build());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));


        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(ShipmentDetails.builder().eventsList(eventsList).build());

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);


        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoListForAttachListShipment(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> result = shipmentService.attachListShipment(commonRequestModel);
        assertEquals(expectedResponse, result);
    }


    @Test
    void createShipmentPayloadAwbTest() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setIsSummaryUpdated(true);
        shipmentDetails.setAdditionalDetails(additionalDetails);
        Containers containers = Containers.builder().build();
        containers.setId(1L);
        PackingResponse packing = new PackingResponse();
        packing.setContainerId(1L);
        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder()
                .id(1L)
                .containersList(new HashSet<>(List.of()))
                .packingList(Arrays.asList(packing))
                .truckDriverDetails(Arrays.asList(TruckDriverDetailsResponse.builder().containerId(1L).build()))
                .build();

        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
     //   when(awbDao.findByShipmentId(any())).thenReturn(Arrays.asList(Awb.builder().airMessageStatus(AwbStatus.AIR_MESSAGE_SENT).build()));
        shipmentService.createShipmentPayload(shipmentDetails, shipmentDetailsResponse, true);
        verify(masterDataUtils, atLeastOnce()).withMdc(any());
    }

    @Test
    void retrieveByIdGuidNull() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        assertEquals(HttpStatus.BAD_REQUEST, shipmentService.retrieveById(commonRequestModel).getStatusCode());
    }

    @Test
    void retrieveByIdNull() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid(UUID.randomUUID().toString()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.empty());
        assertEquals(HttpStatus.BAD_REQUEST, shipmentService.retrieveById(commonRequestModel).getStatusCode());
    }

    @Test
    void retrieveByIdAsycEmptyShipment() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid(UUID.randomUUID().toString()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        CompletableFuture<ResponseEntity<IRunnerResponse>> httpResponse = shipmentService.retrieveByIdAsync(commonRequestModel);
        httpResponse.whenComplete((responseEntity, throwable) -> {
            assertTrue(throwable instanceof CompletionException);
            assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
        });
    }

    @Test
    void retrieveByIdAsycShipment() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid(UUID.randomUUID().toString()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(ShipmentDetails.builder().build()));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(ShipmentDetailsResponse.builder().build());
        CompletableFuture<ResponseEntity<IRunnerResponse>> httpResponse = shipmentService.retrieveByIdAsync(commonRequestModel);
        httpResponse.whenComplete((responseEntity, throwable) -> {
            assertTrue(throwable instanceof CompletionException);
            assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
        });
    }

    @Test
    void completeV1ShipmentCreateAndUpdatePackingRequestListNotNullAir() throws RunnerException {
        ShipmentDetailsResponse mockShipmentResponse = new ShipmentDetailsResponse();
        mockShipmentResponse.setId(1L);
        mockShipmentResponse.setShipmentId("AIR-CAN-00001");

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        List<PackingRequest> packingRequestList = Arrays.asList(PackingRequest.builder().build());
        List<Packing> packingList= new ArrayList<>();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder()
                .id(1L)
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(consolidationDetails)))
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .packingList(packingRequestList).build();

        CommonRequestModel request = CommonRequestModel.builder().data(shipmentRequest).build();
        ShipmentDetails shipmentDetails = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consoleDetails));

        when(mockObjectMapper.convertValue(shipmentRequest, ShipmentDetails.class)).thenReturn(shipmentDetails);
        when(shipmentDao.update(shipmentDetails, true)).thenReturn(shipmentDetails);

        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(mockShipmentResponse);

        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(packingDao.updateEntityFromShipment(anyList(), anyLong(), anyList(), any(), any(), any())).thenReturn(packingList);
        when(jsonHelper.convertValueToList(anyList(), any())).thenReturn(List.of());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeV1ShipmentCreateAndUpdate(request, new HashMap<UUID, String>(), new ArrayList<NotesRequest>(), true, new ArrayList<AuditLogRequestV2>(), "user");
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void assignAllContainerShipmentListTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).isConsolidator(false).build());
        ContainerAssignListRequest containerAssignListRequest = new ContainerAssignListRequest();
        containerAssignListRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(containerAssignListRequest).build();
        PageImpl<Containers> containersPage = new PageImpl<>(Arrays.asList(Containers.builder()
                .allocatedWeight(BigDecimal.TEN)
                .achievedWeight(BigDecimal.TEN)
                .allocatedVolume(BigDecimal.TEN)
                .achievedWeightUnit(Constants.WEIGHT_UNIT_KG)
                .achievedVolumeUnit(Constants.VOLUME_UNIT_M3)
                .allocatedWeightUnit(Constants.WEIGHT_UNIT_KG)
                .allocatedVolumeUnit(Constants.VOLUME_UNIT_M3)
                .shipmentsList(new HashSet<>(Arrays.asList(ShipmentDetails.builder().shipmentType(Constants.SHIPMENT_TYPE_LCL).build())))
                .build()));

        when(containerDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(containersPage);
        when(shipmentsContainersMappingDao.findByContainerId(any())).thenReturn(Arrays.asList(ShipmentsContainersMapping.builder().shipmentId(1L).build()));
        when(shipmentDao.findById(any())).thenReturn(Optional.ofNullable(ShipmentDetails.builder().transportMode(Constants.TRANSPORT_MODE_SEA).shipmentType(Constants.CARGO_TYPE_FCL).build()));
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.assignAllContainers(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void assignAllContainerShipmentListFCLTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).isConsolidator(false).build());
        ContainerAssignListRequest containerAssignListRequest = new ContainerAssignListRequest();
        containerAssignListRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(containerAssignListRequest).build();
        PageImpl<Containers> containersPage = new PageImpl<>(Arrays.asList(Containers.builder()
                .allocatedWeight(BigDecimal.TEN)
                .achievedWeight(BigDecimal.TEN)
                .allocatedVolume(BigDecimal.TEN)
                .achievedWeightUnit(Constants.WEIGHT_UNIT_KG)
                .achievedVolumeUnit(Constants.VOLUME_UNIT_M3)
                .allocatedWeightUnit(Constants.WEIGHT_UNIT_KG)
                .allocatedVolumeUnit(Constants.VOLUME_UNIT_M3)
                .shipmentsList(new HashSet<>(Arrays.asList(ShipmentDetails.builder().shipmentType(Constants.CARGO_TYPE_FCL).build())))
                .build()));

        when(containerDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(containersPage);
        when(shipmentsContainersMappingDao.findByContainerId(any())).thenReturn(Arrays.asList(ShipmentsContainersMapping.builder().shipmentId(1L).build()));
        when(shipmentDao.findById(any())).thenReturn(Optional.ofNullable(ShipmentDetails.builder().transportMode(Constants.TRANSPORT_MODE_SEA).shipmentType(Constants.CARGO_TYPE_FCL).build()));
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.assignAllContainers(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void fetchCreditLimitResponseNull() throws RunnerException {
        String orgCode = "ORG123";
        String addressCode = "Default1";

        V1DataResponse mockV1Response = new V1DataResponse();
        CreditLimitResponse mockCreditLimitResponse = new CreditLimitResponse();
        mockV1Response.setEntities(List.of(mockCreditLimitResponse));
        when(v1Service.fetchCreditLimit(any())).thenReturn(V1DataResponse.builder().build());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fetchCreditLimit(orgCode, addressCode);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void fetchCreditLimitResponseNotNull() throws RunnerException {
        String orgCode = "ORG123";
        String addressCode = "Default1";

        V1DataResponse mockV1Response = new V1DataResponse();
        CreditLimitResponse mockCreditLimitResponse = new CreditLimitResponse();
        mockV1Response.setEntities(List.of(mockCreditLimitResponse));
        when(v1Service.fetchCreditLimit(any())).thenReturn(mockV1Response);
        when(jsonHelper.convertValueToList(any(), eq(CreditLimitResponse.class))).thenReturn(null);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fetchCreditLimit(orgCode, addressCode);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
    }

    @Test
    void fetchShipmentsForConsoleIdNull() throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().id(null).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        assertThrows(RunnerException.class, () -> {
            shipmentService.fetchShipmentsForConsoleId(commonRequestModel);
        });
    }

    @Test
    void testCreateConsolidationRoutes() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().shipConsolidationContainerEnabled(true).consolidationLite(false).build());

        PartyRequestV2 partyRequestV2 = new PartyRequestV2();
        partyRequestV2.setTenantId(1);

        Parties parties = Parties.builder().orgCode("1").build();

        doNothing().when(consolidationService).generateConsolidationNumber(any(ConsolidationDetails.class));
        CarrierDetails carrierDetails = CarrierDetails.builder().originPort("OriginPort").destinationPort("DestinationPort").build();
        Routings routings = new Routings();
        routings.setTenantId(1);
        routings.setMode(Constants.TRANSPORT_MODE_SEA);
        routings.setLeg(1L);
        when(jsonHelper.convertValue(any(), eq(Routings.class))).thenReturn(routings);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .carrierDetails(carrierDetails)
                .direction(Constants.DIRECTION_IMP)
                .masterBill("1234")
                .routingsList(Arrays.asList(routings))
                .build();
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(carrierDetails);

        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().carrierDetails(carrierDetails).sendingAgent(parties).receivingAgent(parties).build();
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), eq(false), anyBoolean())).thenReturn(consolidationDetails);
        mockShipmentSettings();
        ConsolidationDetails result = shipmentService.createConsolidation(shipmentDetails, new ArrayList<>());

        assertNotNull(result);
        assertEquals(carrierDetails, result.getCarrierDetails());
    }

    @Test
    void calculateAutoUpdateWeightVolumeInShipmentPackingListTest() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder()
                .weightChargeableUnit(Constants.WEIGHT_UNIT_KG)
                .volumeChargeableUnit(Constants.VOLUME_UNIT_M3)
                .build());

        AutoUpdateWtVolRequest autoUpdateWtVolRequest = new AutoUpdateWtVolRequest();
        autoUpdateWtVolRequest.setPackingList(Arrays.asList(PackingRequest.builder().build()));

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(autoUpdateWtVolRequest).build();
        AutoUpdateWtVolResponse autoUpdateWtVolResponse = new AutoUpdateWtVolResponse();

        List<Packing> packingList = new ArrayList<>();
        Packing packing = new Packing();
        packing.setPacks("10");
        packingList.add(packing);

        when(jsonHelper.convertValue(autoUpdateWtVolRequest, AutoUpdateWtVolResponse.class)).thenReturn(autoUpdateWtVolResponse);
        when(jsonHelper.convertValueToList(anyList(), eq(Packing.class))).thenReturn(packingList);
        when(packingService.calculatePackSummary(any(), any(), any(), any())).thenReturn(new PackSummaryResponse());
        mockShipmentSettings();
        mockTenantSettings();
        assertNotNull(shipmentService.calculateAutoUpdateWtVolInShipment(commonRequestModel));
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.calculateAutoUpdateWtVolInShipment(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(autoUpdateWtVolResponse), httpResponse);
    }

    @Test
    void changeConsolidationDGValuesById() {
        ShipmentService spyService = spy(shipmentService);
        doReturn(testConsol).when(spyService).saveConsolidationDGValue(false, testConsol);
        ConsolidationDetails consolidationDetails = spyService.changeConsolidationDGValues(false, new AtomicBoolean(true), 1L, shipmentDetails, testConsol);
        assertNotNull(consolidationDetails);
        assertEquals(testConsol, consolidationDetails);
    }

    @Test
    void checkIfAllShipmentsAreNonDG() {
        when(shipmentDao.findByShipmentIdInAndContainsHazardous(any(), anyBoolean())).thenReturn(List.of(shipmentDetails));
        boolean response = shipmentService.checkIfAllShipmentsAreNonDG(List.of(1L));
        assertFalse(response);
    }

    @Test
    void checkIfAllShipmentsAreNonDG_ReturnNull() {
        boolean response = shipmentService.checkIfAllShipmentsAreNonDG(List.of(1L));
        assertTrue(response);
    }

    @Test
    void checkIfAllShipmentsAreNonDG_ReturnEmpty() {
        boolean response = shipmentService.checkIfAllShipmentsAreNonDG(List.of(1L));
        assertTrue(response);
    }

    @Test
    void checkIfAllShipmentsAreNonDG_Empty() {
        boolean response = shipmentService.checkIfAllShipmentsAreNonDG(new ArrayList<>());
        assertTrue(response);
    }

    @Test
    void checkAttachDgAirShipments() {
        testConsol.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        boolean response = shipmentService.checkAttachDgAirShipments(testConsol);
        assertTrue(response);
    }

    @Test
    void checkAttachDgAirShipments_HazTrue() {
        testConsol.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        testConsol.setHazardous(true);
        mockShipmentSettings();
        boolean response = shipmentService.checkAttachDgAirShipments(testConsol);
        assertTrue(response);
    }

    @Test
    void checkAttachDgAirShipments_HazTrue_Settings_AirDgTrue() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        testConsol.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        testConsol.setHazardous(true);
        mockShipmentSettings();
        boolean response = shipmentService.checkAttachDgAirShipments(testConsol);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(false);
        assertFalse(response);
    }

    @Test
    void checkAttachDgAirShipments_HazTrue_Settings_AirDgTrue_EmptyShipment() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        testConsol.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        testConsol.setHazardous(true);
        testConsol.setShipmentsList(new HashSet<>());
        mockShipmentSettings();
        boolean response = shipmentService.checkAttachDgAirShipments(testConsol);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(false);
        assertFalse(response);
    }

    @Test
    void checkAttachDgAirShipments_HazTrue_Settings_AirDgTrue_WithShipment() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        testConsol.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        testConsol.setHazardous(true);
        testConsol.setShipmentsList(Set.of(shipmentDetails));
        mockShipmentSettings();
        boolean response = shipmentService.checkAttachDgAirShipments(testConsol);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(false);
        assertFalse(response);
    }

    @Test
    void createShipmentInV2TestNotesNotNull() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());
        PackingRequest packingRequest = PackingRequest.builder().build();
        CustomerBookingRequest customerBookingRequest = CustomerBookingRequest.builder().id(1L).cargoType(Constants.CARGO_TYPE_FCL).carrierDetails(CarrierDetailRequest.builder().build()).build();
        customerBookingRequest.setPackingList(Collections.singletonList(packingRequest));
        customerBookingRequest.setCustomer(PartiesRequest.builder().addressCode("code").orgCode("org").addressData(new HashMap<>()).orgData(new HashMap<>()).build());

        when(notesDao.findByEntityIdAndEntityType(any(), any())).thenReturn(Arrays.asList(Notes.builder().build()));

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().shipmentId("AIR-CAN-00001")
                .additionalDetails(new AdditionalDetails())
                .carrierDetails(CarrierDetails.builder().build())
                .build();
        shipmentDetails.setGuid(UUID.randomUUID());
        ConsolidationDetailsResponse mockConsolidationDetailsResponse = new ConsolidationDetailsResponse();

        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolRequest.class))).thenReturn(new AutoUpdateWtVolRequest());
        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolResponse.class))).thenReturn(new AutoUpdateWtVolResponse());

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails);
        when(notesDao.findByEntityIdAndEntityType(any(), any())).thenReturn(Arrays.asList(Notes.builder().build()));

        when(jsonHelper.convertValue(any(), eq(Notes.class))).thenReturn(Notes.builder().build());
        when(notesDao.save(any())).thenReturn(Notes.builder().build());

        ShipmentDetailsResponse shipmentDetailsResponse = jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
        when(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(shipmentDetailsResponse);
        mockShipmentSettings();
        mockTenantSettings();
        commonUtils.getShipmentSettingFromContext().setEnableRouteMaster(false);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.createShipmentInV2(customerBookingRequest);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void partialUpdateTestFromV1() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).build();
        shipmentPatchRequest.setCarrierDetails(CarrierPatchRequest.builder().build());
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsolidationDetailsRequest consolidationDetails = ConsolidationDetailsRequest.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ConsolidationDetails consoleDetails = objectMapper.convertValue(consolidationDetails, ConsolidationDetails.class);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .build();

        shipmentDetails.setGuid(UUID.randomUUID());

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());

        when(shipmentDao.update(any(), eq(false))).thenReturn(shipmentDetails);
        mockTenantSettings();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, null);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);

    }

    @Test
    void create_successBranch() throws RunnerException {
        UserContext.getUser().setPermissions(new HashMap<>());
        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(0L).build();
        ShipmentDetails mockShipment = shipmentDetails;
        shipmentDetails.setReceivingBranch(0L);
        shipmentDetails.setTriangulationPartnerList(List.of(triangulationPartner));
        shipmentDetails.setDocumentationPartner(0L);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.getAdditionalDetails().setDraftPrinted(true);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);

        mockShipment.setShipmentId("AIR-CAN-00001");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);
        when(hblDao.findByShipmentId(any())).thenReturn(Collections.emptyList());
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.create(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void create_successBranch_ShipmentPackStatusAndDate() throws RunnerException {
        UserContext.getUser().setPermissions(new HashMap<>());
        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(0L).build();
        ShipmentDetails mockShipment = shipmentDetails;
        shipmentDetails.setReceivingBranch(0L);
        shipmentDetails.setTriangulationPartnerList(List.of(triangulationPartner));
        shipmentDetails.setDocumentationPartner(0L);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.getAdditionalDetails().setDraftPrinted(true);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipmentDetails.getCarrierDetails().setAtd(LocalDateTime.now());
        Packing packing = new Packing();
        packing.setCargoGateInDate(LocalDateTime.now());
        packing.setDateType(DateBehaviorType.ACTUAL);
        shipmentDetails.getPackingList().add(packing);

        mockShipment.setShipmentId("AIR-CAN-00001");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).enableLclConsolidation(true).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);
        when(hblDao.findByShipmentId(any())).thenReturn(Collections.emptyList());
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.create(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void create_successBranchHblNotEmpty() throws RunnerException {
        UserContext.getUser().setPermissions(new HashMap<>());
        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(0L).build();
        ShipmentDetails mockShipment = shipmentDetails;
        shipmentDetails.setReceivingBranch(0L);
        shipmentDetails.setTriangulationPartnerList(List.of(triangulationPartner));
        shipmentDetails.setDocumentationPartner(0L);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.getAdditionalDetails().setDraftPrinted(true);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);

        mockShipment.setShipmentId("AIR-CAN-00001");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);
        when(hblDao.findByShipmentId(any())).thenReturn(Arrays.asList(Hbl.builder().build()));
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.create(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    private Runnable mockRunnable() {
        return null;
    }

    private List<IRunnerResponse> convertEntityListToFullShipmentList(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shipmentDetail -> {
            ShipmentDetailsResponse response = modelMapper.map(shipmentDetail, ShipmentDetailsResponse.class);
            responseList.add(response);
        });
        return responseList;
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<ShipmentListResponse> shipmentListResponses  = ShipmentMapper.INSTANCE.toShipmentListResponses(lst);
        for(var i: shipmentListResponses) {
            setEventData(i);
            if (i.getStatus() != null && i.getStatus() < ShipmentStatus.values().length)
                i.setShipmentStatus(ShipmentStatus.values()[i.getStatus()].toString());
            if (ObjectUtils.isNotEmpty(i.getShipmentOrders()))
                i.setOrdersCount(i.getShipmentOrders().size());
            responseList.add(i);
        }

        return responseList;
    }

    private List<IRunnerResponse> convertEntityListToDtoListForAttachListShipment(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<AttachListShipmentResponse> attachListShipmentResponse  = AttachListShipmentMapper.INSTANCE.toAttachListShipmentResponse(lst);
        for(var i: attachListShipmentResponse) {
            if (i.getStatus() != null && i.getStatus() < ShipmentStatus.values().length)
                i.setShipmentStatus(ShipmentStatus.values()[i.getStatus()].toString());
            responseList.add(i);
        }

        return responseList;
    }

    private List<IRunnerResponse> convertEntityToDtoListSimplified(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shipmentDetails -> {
            ShipmentListResponse response = modelMapper.map(shipmentDetails, ShipmentListResponse.class);
            if (shipmentDetails.getStatus() != null && shipmentDetails.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[shipmentDetails.getStatus()].toString());
            responseList.add(response);
        });
        return responseList;
    }

    private void containerCountUpdate(ShipmentDetails shipmentDetail, ShipmentListResponse response) {
        Long container20Count = 0L;
        Long container40Count = 0L;
        Long container20GPCount = 0L;
        Long container20RECount = 0L;
        Long container40GPCount = 0L;
        Long container40RECount = 0L;
        Set<String> containerNumber = new HashSet<>();
        if (shipmentDetail.getContainersList() != null) {
            for (Containers container : shipmentDetail.getContainersList()) {
                if(container.getContainerCode() != null) {
                    if (container.getContainerCode().contains(Constants.Cont20)) {
                        ++container20Count;
                    } else if (container.getContainerCode().contains(Constants.Cont40)) {
                        ++container40Count;
                    } else if (container.getContainerCode().equals(Constants.Cont20GP)) {
                        ++container20GPCount;
                    } else if (container.getContainerCode().equals(Constants.Cont20RE)) {
                        ++container20RECount;
                    } else if (container.getContainerCode().equals(Constants.Cont40GP)) {
                        ++container40GPCount;
                    } else if (container.getContainerCode().equals(Constants.Cont40RE)) {
                        ++container40RECount;
                    }
                }
                if (StringUtility.isNotEmpty(container.getContainerNumber())) {
                    containerNumber.add(container.getContainerNumber());
                }
            }
//            container20Count = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().contains(Constants.Cont20)).count();
//            container40Count = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().contains(Constants.Cont40)).count();
//            container20GPCount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont20GP)).count();
//            container20RECount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont20RE)).count();
//            container40GPCount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont40GP)).count();
//            container40RECount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont40RE)).count();
        }
        response.setContainer20Count(container20Count);
        response.setContainer40Count(container40Count);
        response.setContainer20GPCount(container20GPCount);
        response.setContainer20RECount(container20RECount);
        response.setContainer40GPCount(container40GPCount);
        response.setContainer40RECount(container40RECount);
        response.setContainerNumbers(containerNumber);
    }
    private void setEventData(ShipmentListResponse response) {
        if (response.getEventsList() != null) {
            for (EventsResponse events : response.getEventsList()) {
                if (StringUtility.isNotEmpty(events.getEventCode())) {
                    if (events.getEventCode().equalsIgnoreCase(EventConstants.INVGNTD)) {
                        response.setInvoiceDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(EventConstants.TAXSG)) {
                        response.setTaxDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(EventConstants.CSEDI)) {
                        response.setCustomsFilingDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(EventConstants.AMSEDI)) {
                        response.setAmsFilingDate(events.getActual());
                    }
                }
            }
        }
    }

    @Test
    void testGetDateTimeChangeUpdatesThrowsExceptionWhenShipmentIdIsNull() {
        Long shipmentId = null;
        assertThrows(RunnerException.class,  () -> shipmentService.getDateTimeChangeUpdates(shipmentId));
    }
    @Test
    void testGetDateTimeChangeUpdatesThrowsExceptionWhenShipmentNotPresent() {
        Long shipmentId = 1L;
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.empty());
        assertThrows(RunnerException.class,  () -> shipmentService.getDateTimeChangeUpdates(shipmentId));
    }



    @Test
    void testGetDateTimeChangeUpdates() throws RunnerException {
        Long shipmentId = 1L;

        TrackingServiceApiResponse trackingResponse = new TrackingServiceApiResponse();
        trackingResponse.setContainers(List.of(TrackingServiceApiResponse.Container.builder()
            .journey(new TrackingServiceApiResponse.Journey()).build()));

        LocalDateTime mockDateTime = LocalDateTime.now();
        List<DateTimeChangeLog> shipmentDateLogs = new ArrayList<>();
        DateTimeChangeLog ataDateTimeChangeLog = DateTimeChangeLog.builder()
            .dateType(DateType.ATA)
            .currentValue(LocalDateTime.now())
            .sourceOfUpdate("Tracking Service")
            .shipmentId(1L)
            .build();
        ataDateTimeChangeLog.setUpdatedAt(LocalDateTime.now().minusDays(3));

        DateTimeChangeLog atdDateTimeChangeLog = DateTimeChangeLog.builder()
            .dateType(DateType.ATD)
            .currentValue(LocalDateTime.now())
            .sourceOfUpdate("Tracking Service")
            .shipmentId(1L)
            .build();
        atdDateTimeChangeLog.setUpdatedAt(LocalDateTime.now().minusDays(3));

        DateTimeChangeLog etaDateTimeChangeLog = DateTimeChangeLog.builder()
            .dateType(DateType.ETA)
            .currentValue(LocalDateTime.now())
            .sourceOfUpdate("Tracking Service")
            .shipmentId(1L)
            .build();
        etaDateTimeChangeLog.setUpdatedAt(LocalDateTime.now().minusDays(3));

        DateTimeChangeLog etdDateTimeChangeLog = DateTimeChangeLog.builder()
            .dateType(DateType.ETD)
            .currentValue(LocalDateTime.now())
            .sourceOfUpdate("Tracking Service")
            .shipmentId(1L)
            .build();
        etdDateTimeChangeLog.setUpdatedAt(LocalDateTime.now().minusDays(3));

        shipmentDateLogs.addAll(List.of(ataDateTimeChangeLog, atdDateTimeChangeLog, etaDateTimeChangeLog, etdDateTimeChangeLog));

        TrackingServiceApiResponse.DateAndSources dateAndSources = new TrackingServiceApiResponse.DateAndSources();
        dateAndSources.setDateTime(mockDateTime);

        trackingResponse.getContainers().get(0).getJourney().setPortOfArrivalAta(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfDepartureAtd(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfArrivalEta(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfDepartureEtd(dateAndSources);

        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));
        when(trackingServiceAdapter.fetchTrackingData(any())).thenReturn(trackingResponse);
        when(dateTimeChangeLogService.getDateTimeChangeLog(shipmentId)).thenReturn(shipmentDateLogs);


        var res = shipmentService.getDateTimeChangeUpdates(shipmentId);

        assertEquals(HttpStatus.OK, res.getStatusCode());
    }

    @Test
    void testConsoleShipmentList_Success() throws AuthenticationException {
        Criteria criteria = Criteria.builder().fieldName("wayBillNumber").value(1).build();
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(Arrays.asList(FilterCriteria.builder().criteria(criteria).innerFilter(new ArrayList<>()).build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setInterBranchConsole(true);
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder().shipmentId(2L).consolidationId(1L)
                .isAttachmentDone(false).requestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED).build();

        GuidsListResponse guidsListResponse = new GuidsListResponse();
        when(v1Service.fetchWayBillNumberFilterGuids(any())).thenReturn(guidsListResponse);

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(2L);
        shipmentDetailsList.add(shipmentDetails);
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationIdAll(1L)).thenReturn(Arrays.asList(consoleShipmentMapping));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.consoleShipmentList(commonRequestModel, 1L, null,false, true, false);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }
    @Test
    void testConsoleShipmentList_SuccessForNte() throws AuthenticationException {
        Criteria criteria = Criteria.builder().fieldName("wayBillNumber").value(1).build();
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(Arrays.asList(FilterCriteria.builder().criteria(criteria).innerFilter(new ArrayList<>()).build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.setReceivingBranch(23L);
        TenantContext.setCurrentTenant(23);
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder().shipmentId(2L).consolidationId(1L)
                .isAttachmentDone(false).requestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED).build();

        GuidsListResponse guidsListResponse = new GuidsListResponse();
        when(v1Service.fetchWayBillNumberFilterGuids(any())).thenReturn(guidsListResponse);

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(2L);
        shipmentDetailsList.add(shipmentDetails);
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        when(consolidationDetailsDao.findConsolidationByIdWithQuery(1L)).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationIdAll(1L)).thenReturn(Arrays.asList(consoleShipmentMapping));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.consoleShipmentList(commonRequestModel, 1L, null,false, true, true);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testConsoleShipmentList_WithGuidSuccess() throws AuthenticationException {
        Criteria criteria = Criteria.builder().fieldName("wayBillNumber").value(1).build();
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(Collections.singletonList(FilterCriteria.builder().criteria(criteria).innerFilter(new ArrayList<>()).build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();
        ConsolidationDetails consoleDetails = new ConsolidationDetails();
        consoleDetails.setId(1L);
        consoleDetails.setInterBranchConsole(true);
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder().shipmentId(2L).consolidationId(1L)
                .isAttachmentDone(false).requestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED).build();

        GuidsListResponse guidsListResponse = new GuidsListResponse();
        when(v1Service.fetchWayBillNumberFilterGuids(any())).thenReturn(guidsListResponse);

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        ShipmentDetails shipmentData = new ShipmentDetails();
        shipmentData.setId(2L);
        shipmentDetailsList.add(shipmentData);
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consoleDetails));
        when(consoleShipmentMappingDao.findByConsolidationIdAll(1L)).thenReturn(Collections.singletonList(consoleShipmentMapping));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.consoleShipmentList(commonRequestModel, null, "382bd0b8-e5e4-4482-b599-2e53a52cf7f3",false, true, false);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testConsoleShipmentList_Error() {
        Criteria criteria = Criteria.builder().fieldName("wayBillNumber").value(1).build();
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(Collections.singletonList(FilterCriteria.builder().criteria(criteria).innerFilter(new ArrayList<>()).build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();
        assertThrows(ValidationException.class,  () ->  shipmentService.consoleShipmentList(commonRequestModel, null, null,false, true, false));
    }

    @Test
    void testConsoleShipmentList_Success_IsAttachedTrue() throws AuthenticationException {
        Criteria criteria = Criteria.builder().fieldName("wayBillNumber").value(1).build();
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(Arrays.asList(FilterCriteria.builder().criteria(criteria).innerFilter(new ArrayList<>()).build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setInterBranchConsole(true);

        GuidsListResponse guidsListResponse = new GuidsListResponse();
        when(v1Service.fetchWayBillNumberFilterGuids(any())).thenReturn(guidsListResponse);

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(2L);
        shipmentDetailsList.add(shipmentDetails);
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.consoleShipmentList(commonRequestModel, 1L, null, true, true, false);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testConsoleShipmentList_Success_InterBranchFalse() throws AuthenticationException {
        Criteria criteria = Criteria.builder().fieldName("wayBillNumber").value(1).build();
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(Arrays.asList(FilterCriteria.builder().criteria(criteria).innerFilter(new ArrayList<>()).build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setInterBranchConsole(false);

        GuidsListResponse guidsListResponse = new GuidsListResponse();
        when(v1Service.fetchWayBillNumberFilterGuids(any())).thenReturn(guidsListResponse);

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(2L);
        shipmentDetailsList.add(shipmentDetails);
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.consoleShipmentList(commonRequestModel, 1L, null, true, true, false);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testConsoleShipmentList_Failure_DataRetrieveFailureConsole() {
        Criteria criteria = Criteria.builder().fieldName("wayBillNumber").value(1).build();
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(Arrays.asList(FilterCriteria.builder().criteria(criteria).innerFilter(new ArrayList<>()).build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class,  () -> shipmentService.consoleShipmentList(commonRequestModel, 1L, null, true, true, false));
    }

    @Test
    void testConsoleShipmentList_Failure_NullRequest() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setInterBranchConsole(false);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        assertThrows(ValidationException.class,  () -> shipmentService.consoleShipmentList(commonRequestModel, 1L, null, true, true, false));
    }


    @Test
    void testUpdateConsolAchievedAndOpenForAttachmentFromShipmentUpdate() throws RunnerException {
        shipmentDetails.setId(1L);
        ConsolidationDetails attachedConsol = new ConsolidationDetails();
        attachedConsol.setId(1L);
        shipmentDetails.setConsolidationList(Set.of(attachedConsol));
        ShipmentDetails mockShipment = shipmentDetails;
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        mockShipmentRequest.setConsolidationAchievedQuantities(new AchievedQuantitiesRequest());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        // Mock
        mockShipmentSettings();
        mockTenantSettings();
        when(shipmentDao.findById(any()))
            .thenReturn(
                Optional.of(
                    shipmentDetails
                        .setConsolidationList(Set.of(attachedConsol))
                        .setContainersList(new HashSet<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
        verify(packingService, times(1)).savePackUtilisationCalculationInConsole(any());
    }

    @Test
    void retrieveByMeasurmentBasisTest() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid(UUID.randomUUID().toString()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        Containers containers = new Containers();
        containers.setContainerCode("20GP");
        containers.setContainerCount(1L);
        shipmentDetails.setContainersList(new HashSet<>(Arrays.asList(containers)));

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(modelMapper.map(any(), any())).thenReturn(MeasurementBasisResponse.builder().build());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.shipmentRetrieveWithMeasurmentBasis(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void retrieveByMeasurmentBasisTestWithIdNull() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.shipmentRetrieveWithMeasurmentBasis(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void retrieveByMeasurmentBasisTest3() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid(UUID.randomUUID().toString()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();

        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(modelMapper.map(any(), any())).thenReturn(MeasurementBasisResponse.builder().build());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.shipmentRetrieveWithMeasurmentBasis(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testGetAllShipments_Success_CurrentBranch() {
        // Setup
        Long consoleId = 1L;
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setInterBranchConsole(false);
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setRequestedType(null);
        consoleShipmentMappingList.add(consoleShipmentMapping);

        when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationIdAll(consoleId)).thenReturn(consoleShipmentMappingList);

        // Execute
        ResponseEntity<IRunnerResponse> response = shipmentService.getAllShipments(consoleId);

        // Verify
        verify(commonUtils, never()).setInterBranchContextForHub();
        verify(consolidationDetailsDao).findById(consoleId);
        verify(consoleShipmentMappingDao).findByConsolidationIdAll(consoleId);

        // Assert
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testGetAllShipments_Success_InterBranch_Push() {
        // Setup
        Long consoleId = 1L;
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setInterBranchConsole(false);
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setRequestedType(ShipmentRequestedType.SHIPMENT_PUSH_ACCEPTED);
        consoleShipmentMappingList.add(consoleShipmentMapping);

        when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationIdAll(consoleId)).thenReturn(consoleShipmentMappingList);

        // Execute
        ResponseEntity<IRunnerResponse> response = shipmentService.getAllShipments(consoleId);

        // Verify
        verify(commonUtils, never()).setInterBranchContextForHub();
        verify(consolidationDetailsDao).findById(consoleId);
        verify(consoleShipmentMappingDao).findByConsolidationIdAll(consoleId);

        // Assert
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testGetAllShipments_Success_InterBranch_Pull() {
        // Setup
        Long consoleId = 1L;
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setInterBranchConsole(false);
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setRequestedType(ShipmentRequestedType.SHIPMENT_PULL_ACCEPTED);
        consoleShipmentMappingList.add(consoleShipmentMapping);

        when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationIdAll(consoleId)).thenReturn(consoleShipmentMappingList);

        // Execute
        ResponseEntity<IRunnerResponse> response = shipmentService.getAllShipments(consoleId);

        // Verify
        verify(commonUtils, never()).setInterBranchContextForHub();
        verify(consolidationDetailsDao).findById(consoleId);
        verify(consoleShipmentMappingDao).findByConsolidationIdAll(consoleId);

        // Assert
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testGetAllShipments_Success_PendingAttachment_Push() {
        // Setup
        Long consoleId = 1L;
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setInterBranchConsole(false);
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setRequestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED);
        consoleShipmentMappingList.add(consoleShipmentMapping);

        when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationIdAll(consoleId)).thenReturn(consoleShipmentMappingList);

        // Execute
        ResponseEntity<IRunnerResponse> response = shipmentService.getAllShipments(consoleId);

        // Verify
        verify(commonUtils, never()).setInterBranchContextForHub();
        verify(consolidationDetailsDao).findById(consoleId);
        verify(consoleShipmentMappingDao).findByConsolidationIdAll(consoleId);

        // Assert
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testGetAllShipments_Success_PendingAttachment_Pull() {
        // Setup
        Long consoleId = 1L;
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setInterBranchConsole(false);
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setRequestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED);
        consoleShipmentMappingList.add(consoleShipmentMapping);

        when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationIdAll(consoleId)).thenReturn(consoleShipmentMappingList);

        // Execute
        ResponseEntity<IRunnerResponse> response = shipmentService.getAllShipments(consoleId);

        // Verify
        verify(commonUtils, never()).setInterBranchContextForHub();
        verify(consolidationDetailsDao).findById(consoleId);
        verify(consoleShipmentMappingDao).findByConsolidationIdAll(consoleId);

        // Assert
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }


    @Test
    void testGetAllShipments_DataNotFound() {
        // Setup
        Long consoleId = 1L;
        when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.empty());

        // Execute & Assert
        DataRetrievalFailureException exception = assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentService.getAllShipments(consoleId);
        });

        // Verify
        verify(consolidationDetailsDao).findById(consoleId);
        verify(consoleShipmentMappingDao, never()).findByConsolidationId(consoleId);
        verify(commonUtils, never()).setInterBranchContextForHub();
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, exception.getMessage());
    }

    @Test
    void testUpdateShipments_HubRequest_Approve() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        updateConsoleShipmentRequest.setForHub(true);
        updateConsoleShipmentRequest.setShipmentRequestedType(ShipmentRequestedType.APPROVE);
        updateConsoleShipmentRequest.setConsoleId(1L);
        updateConsoleShipmentRequest.setListOfShipments(List.of(1L));

        consolidationDetails.setInterBranchConsole(true);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(testConsol));
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
        doNothing().when(spyService).sendEmailsForPushRequestAccept(any(), any(), any(), any());

        ResponseEntity<IRunnerResponse> response = spyService.updateShipments(updateConsoleShipmentRequest);

        verify(consolidationService).attachShipments(ShipmentRequestedType.APPROVE, 1L, List.of(1L), true);
        assertNotNull(response);
    }

    @Test
    void testUpdateShipments_HubRequest_Reject() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        updateConsoleShipmentRequest.setForHub(true);
        updateConsoleShipmentRequest.setShipmentRequestedType(ShipmentRequestedType.REJECT);
        updateConsoleShipmentRequest.setConsoleId(1L);
        updateConsoleShipmentRequest.setListOfShipments(List.of(1L));

        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(ConsoleShipmentMapping.builder().build())));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        doNothing().when(spyService).sendEmailForPushRequestReject(any(), any(), any(), any(), any());

        ResponseEntity<IRunnerResponse> response = spyService.updateShipments(updateConsoleShipmentRequest);

        verify(consoleShipmentMappingDao).deletePendingStateByConsoleIdAndShipmentId(1L, 1L);
        assertNotNull(response);
    }

    @Test
    void testUpdateShipments_HubRequest_Withdraw() throws RunnerException {
        updateConsoleShipmentRequest.setForHub(true);
        updateConsoleShipmentRequest.setShipmentRequestedType(ShipmentRequestedType.WITHDRAW);
        updateConsoleShipmentRequest.setConsoleId(1L);
        updateConsoleShipmentRequest.setListOfShipments(List.of(1L));

        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(ConsoleShipmentMapping.builder().build())));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());

        ResponseEntity<IRunnerResponse> response = shipmentService.updateShipments(updateConsoleShipmentRequest);

        verify(consoleShipmentMappingDao).deletePendingStateByConsoleIdAndShipmentId(1L, 1L);
        assertNotNull(response);
    }

    @Test
    void testUpdateShipments_HubRequest_DataRetrievalFailure() {
        updateConsoleShipmentRequest.setForHub(true);
        updateConsoleShipmentRequest.setConsoleId(1L);

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.empty());

        DataRetrievalFailureException exception = assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentService.updateShipments(updateConsoleShipmentRequest);
        });

        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, exception.getMessage());
    }

    @Test
    void testUpdateShipments_NonHubRequest_Approve() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        updateConsoleShipmentRequest.setForHub(false); // Non-hub request
        updateConsoleShipmentRequest.setShipmentRequestedType(ShipmentRequestedType.APPROVE);
        updateConsoleShipmentRequest.setConsoleIdsList(List.of(1L));
        updateConsoleShipmentRequest.setShipmentId(1L);
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().build()))));
        doNothing().when(spyService).sendEmailsForPullRequestAccept(any(), any(), any(), any());

        ResponseEntity<IRunnerResponse> response = spyService.updateShipments(updateConsoleShipmentRequest);

        verify(consolidationService).attachShipments(ShipmentRequestedType.APPROVE, 1L, List.of(1L), false);
        verify(consoleShipmentMappingDao).deletePendingStateByShipmentId(1L);
        assertNotNull(response);
    }

    @Test
    void testUpdateShipments_NonHubRequest_Reject() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        updateConsoleShipmentRequest.setForHub(false);
        updateConsoleShipmentRequest.setShipmentRequestedType(ShipmentRequestedType.REJECT);
        updateConsoleShipmentRequest.setConsoleIdsList(List.of(1L));
        updateConsoleShipmentRequest.setShipmentId(1L);
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(ConsoleShipmentMapping.builder().build())));
        doNothing().when(spyService).sendEmailForPullRequestReject(any(), any(), any(), any(), any());

        ResponseEntity<IRunnerResponse> response = spyService.updateShipments(updateConsoleShipmentRequest);

        verify(consoleShipmentMappingDao).deletePendingStateByConsoleIdAndShipmentId(1L, 1L);
        assertNotNull(response);
    }

    @Test
    void testUpdateShipments_NonHubRequest_Withdraw() throws RunnerException {
        updateConsoleShipmentRequest.setForHub(false);
        updateConsoleShipmentRequest.setShipmentRequestedType(ShipmentRequestedType.WITHDRAW);
        updateConsoleShipmentRequest.setConsoleIdsList(List.of(1L));
        updateConsoleShipmentRequest.setShipmentId(1L);
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(ConsoleShipmentMapping.builder().build())));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());

        ResponseEntity<IRunnerResponse> response = shipmentService.updateShipments(updateConsoleShipmentRequest);

        verify(consoleShipmentMappingDao).deletePendingStateByConsoleIdAndShipmentId(1L, 1L);
        assertNotNull(response);
    }

    @Test
    void testUpdateShipments_NonHubRequest_InvalidDataException() {
        updateConsoleShipmentRequest.setForHub(false);
        updateConsoleShipmentRequest.setConsoleIdsList(null); // Invalid data
        updateConsoleShipmentRequest.setShipmentRequestedType(ShipmentRequestedType.APPROVE);
        updateConsoleShipmentRequest.setShipmentId(1L);

        InvalidDataAccessApiUsageException exception = assertThrows(InvalidDataAccessApiUsageException.class, () -> {
            shipmentService.updateShipments(updateConsoleShipmentRequest);
        });

        assertEquals("Console Ids list should not be empty!!!", exception.getMessage());
    }

    @Test
    void testUpdateShipments_NonHubRequest_InvalidDataException2() {
        updateConsoleShipmentRequest.setForHub(false);
        updateConsoleShipmentRequest.setConsoleIdsList(List.of()); // Invalid data
        updateConsoleShipmentRequest.setShipmentRequestedType(ShipmentRequestedType.APPROVE);
        updateConsoleShipmentRequest.setShipmentId(1L);

        InvalidDataAccessApiUsageException exception = assertThrows(InvalidDataAccessApiUsageException.class, () -> {
            shipmentService.updateShipments(updateConsoleShipmentRequest);
        });

        assertEquals("Console Ids list should not be empty!!!", exception.getMessage());
    }

    @Test
    void testUpdateShipments_HubRequest_AttachShipmentsThrowsRunnerException() throws RunnerException {
        updateConsoleShipmentRequest.setForHub(true);
        updateConsoleShipmentRequest.setShipmentRequestedType(ShipmentRequestedType.APPROVE);
        updateConsoleShipmentRequest.setConsoleId(1L);
        updateConsoleShipmentRequest.setListOfShipments(List.of(1L));

        consolidationDetails.setInterBranchConsole(true);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        doThrow(new RunnerException("Mocked RunnerException")).when(consolidationService).attachShipments(any(), anyLong(), anyList(), anyBoolean());

        RuntimeException exception = assertThrows(RuntimeException.class, () -> {
            shipmentService.updateShipments(updateConsoleShipmentRequest);
        });

        assertNull(exception.getCause());
        assertFalse(exception.getCause() instanceof RunnerException);

        verify(consoleShipmentMappingDao, never()).deletePendingStateByShipmentId(anyLong());
    }

    @Test
    void testGetAllShipments_HubRequest_SetInterBranchContextForHub_True() {
        // Arrange
        Long consoleId = 1L;

        when(consolidationDetails.getInterBranchConsole()).thenReturn(true);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        // Act
        shipmentService.getAllShipments(consoleId);

        // Assert
        verify(commonUtils).setInterBranchContextForHub();
    }

    @Test
    void testGetAllShipments_HubRequest_SetInterBranchContextForHub_False() {
        // Arrange
        Long consoleId = 1L;

        when(consolidationDetails.getInterBranchConsole()).thenReturn(false);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        // Act
        shipmentService.getAllShipments(consoleId);

        // Assert
        verify(commonUtils, never()).setInterBranchContextForHub();
    }

    @Test
    void testUpdateShipments_HubRequest_SetInterBranchContextForHub_True() throws RunnerException {
        // Arrange
        updateConsoleShipmentRequest.setForHub(true);
        updateConsoleShipmentRequest.setConsoleId(1L);

        when(consolidationDetails.getInterBranchConsole()).thenReturn(true);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        // Act
        shipmentService.updateShipments(updateConsoleShipmentRequest);

        // Assert
        verify(commonUtils).setInterBranchContextForHub();
    }

    @Test
    void testUpdateShipments_HubRequest_SetInterBranchContextForHub_False() throws RunnerException {
        // Arrange
        updateConsoleShipmentRequest.setForHub(true);
        updateConsoleShipmentRequest.setConsoleId(1L);

        when(consolidationDetails.getInterBranchConsole()).thenReturn(false);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        // Act
        shipmentService.updateShipments(updateConsoleShipmentRequest);

        // Assert
        verify(commonUtils, never()).setInterBranchContextForHub();
    }

    @Test
    void testRequestInterBranchConsole_Success() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        mockShipmentSettings();
        doNothing().when(spyService).sendEmailForPushRequested(any(), any(), any());
        var response = spyService.requestInterBranchConsole(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testRequestInterBranchConsole_Success_Error() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of(ConsoleShipmentMapping.builder()
                .shipmentId(1L).consolidationId(3L).isAttachmentDone(true).build()));
        var response = spyService.requestInterBranchConsole(1L, 2L, "");
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testRequestInterBranchConsole_ExistingMapping() throws RunnerException {
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of(ConsoleShipmentMapping.builder()
                .shipmentId(1L).consolidationId(2L).build()));
        var response = shipmentService.requestInterBranchConsole(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testRequestInterBranchConsole_InterBranchImportShipment() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .direction(Constants.DIRECTION_IMP)
                .build();
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(ConsolidationDetails.builder().build()));
        mockShipmentSettings();
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        when(commonUtils.getEmailTemplates(anyString())).thenReturn(List.of());
        var response = spyService.requestInterBranchConsole(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(spyService, never()).sendEmailForPushRequested(any(), any(), any());
    }

    @Test
    void testRequestInterBranchConsole_InterBranchImportShipment1() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder()
                .direction(Constants.DIRECTION_IMP)
                .build();
        shipmentDetails1.setCreatedBy("abc");
        shipmentDetails1.setAssignedTo("def");
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails1));
        when(consolidationDetailsDao.findById(2L)).thenReturn(Optional.of(consolidationDetails));
        mockShipmentSettings();
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        when(commonUtils.getEmailTemplates(anyString())).thenReturn(List.of(new EmailTemplatesRequest()));
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        var response = spyService.requestInterBranchConsole(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(spyService, never()).sendEmailForPushRequested(any(), any(), any());
    }

    @Test
    void testRequestInterBranchConsole_InterBranchImportShipment2() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .direction(Constants.DIRECTION_IMP)
                .build();
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        mockShipmentSettings();
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        when(commonUtils.getEmailTemplates(anyString())).thenReturn(null);
        var response = spyService.requestInterBranchConsole(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(spyService, never()).sendEmailForPushRequested(any(), any(), any());
    }

    @Test
    void testRequestInterBranchConsole_InterBranchImportShipment3() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder()
                .direction(Constants.DIRECTION_IMP)
                .build();
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails1));
        when(consolidationDetailsDao.findById(2L)).thenReturn(Optional.of(consolidationDetails));
        mockShipmentSettings();
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        when(commonUtils.getEmailTemplates(anyString())).thenReturn(List.of(new EmailTemplatesRequest()));
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        var response = spyService.requestInterBranchConsole(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(spyService, never()).sendEmailForPushRequested(any(), any(), any());
    }

    @Test
    void testRequestInterBranchConsole_NonInterBranchShipment_EmailSent() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .direction("EXP")
                .build();
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(ConsolidationDetails.builder().build()));
        mockShipmentSettings();
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        doNothing().when(spyService).sendEmailForPushRequested(any(), any(), any());
        var response = spyService.requestInterBranchConsole(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(consoleShipmentMappingDao).save(argThat(entity ->
                entity.getIsAttachmentDone() != null && !entity.getIsAttachmentDone()
        ));
        verify(spyService).sendEmailForPushRequested(any(), any(), any());
    }

    @Test
    void testGetPendingNotificationsSuccess() {
        PendingNotificationRequest request = new PendingNotificationRequest();
        request.setShipmentIdList(List.of(1L,2L));

        ConsolidationDetails mockConsol1 = jsonTestUtility.getTestConsolidation();
        mockConsol1.setTenantId(1);
        mockConsol1.setId(1L);
        var mockConsol2 = objectMapper.convertValue(mockConsol1, ConsolidationDetails.class);
        mockConsol2.setId(2L);
        var mockConsol3 = objectMapper.convertValue(mockConsol1, ConsolidationDetails.class);
        mockConsol3.setId(3L);

        V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
        tenantSettingsResponse.setIsMAWBColoadingEnabled(true);
        tenantSettingsResponse.setIsColoadingMAWBStationEnabled(true);

        List<ConsoleShipmentMapping> mappings = new ArrayList<>();
        mappings.add(ConsoleShipmentMapping.builder()
            .shipmentId(1L).consolidationId(1L).requestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED).isAttachmentDone(false).build());
        mappings.add(ConsoleShipmentMapping.builder()
            .shipmentId(1L).consolidationId(2L).requestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED).isAttachmentDone(false).build());
        mappings.add(ConsoleShipmentMapping.builder()
            .shipmentId(2L).consolidationId(1L).requestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED).isAttachmentDone(false).build());
        mappings.add(ConsoleShipmentMapping.builder()
            .shipmentId(2L).consolidationId(3L).requestedType(ShipmentRequestedType.SHIPMENT_PULL_REQUESTED).isAttachmentDone(false).build());

        List<ConsolidationDetails> consolidationDetailsList = List.of(mockConsol1, mockConsol2, mockConsol3);

        // mocking
        TenantSettingsDetailsContext.setCurrentTenantSettings(tenantSettingsResponse);
        mockTenantSettings();
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(mappings));
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(consolidationDetailsList));
        // Test
        var httpResponse = shipmentService.getPendingNotifications(CommonRequestModel.buildRequest(request));
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        var runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        JavaType javaType = objectMapper.getTypeFactory().constructParametricType(PendingNotificationResponse.class, PendingShipmentActionsResponse.class);
        PendingNotificationResponse<PendingShipmentActionsResponse> responseBody = objectMapper.convertValue(runnerResponse.getData(), javaType);
        assertEquals(2, responseBody.getNotificationMap().size()); // number of shipments with pending notifications
        assertEquals(2, responseBody.getNotificationMap().get(1L).size()); // notification count of shipment with id 1L
    }

    @Test
    void testGetPendingNotificationsReturnsEmptyResponseIfTenantSettingsNotEnabled() {
        PendingNotificationRequest request = new PendingNotificationRequest();
        request.setShipmentIdList(List.of(1L,2L));

        V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
        TenantSettingsDetailsContext.setCurrentTenantSettings(tenantSettingsResponse);

        // Mock
        mockTenantSettings();
        // Test
        var httpResponse = shipmentService.getPendingNotifications(CommonRequestModel.buildRequest(request));
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        var responseBody = objectMapper.convertValue(httpResponse.getBody(), PendingNotificationResponse.class);
        assertNull(responseBody.getNotificationMap());
    }

    @Test
    void testGetPendingNotificationsReturnsEmptyResponseForEmptyList() {
        PendingNotificationRequest request = new PendingNotificationRequest();
        request.setShipmentIdList(null);

        var httpResponse = shipmentService.getPendingNotifications(CommonRequestModel.buildRequest(request));

        PendingNotificationResponse mockResponse  = new PendingNotificationResponse();

        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);
    }

    @Test
    void testGetLatestCargoDeliveryDate_Success() {
        // Arrange
        Long consoleId = 1L;
        ConsolidationDetails consolidationDetails = mock(ConsolidationDetails.class);
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setCargoDeliveryDate(LocalDateTime.of(2023, 8, 15, 10, 0));
        ShipmentDetails shipmentDetails2 = new ShipmentDetails();
        shipmentDetails2.setCargoDeliveryDate(LocalDateTime.of(2023, 8, 16, 10, 0));
        Set<ShipmentDetails> shipmentDetailsList = new HashSet<>(List.of(shipmentDetails1, shipmentDetails2));
        when(consolidationDetails.getShipmentsList()).thenReturn(shipmentDetailsList);
        when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolidationDetails));

        // Act
        ResponseEntity<IRunnerResponse> response = shipmentService.getLatestCargoDeliveryDate(consoleId);

        // Assert
        assertNotNull(response);
        assertEquals(200, response.getStatusCodeValue());
    }

    @Test
    void testGetLatestCargoDeliveryDate_ConsolidationNotFound() {
        // Arrange
        Long consoleId = 1L;
        when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.empty());

        // Act & Assert
        DataRetrievalFailureException exception = assertThrows(DataRetrievalFailureException.class,
                () -> shipmentService.getLatestCargoDeliveryDate(consoleId));

        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, exception.getMessage());
    }

    @Test
    void testGetLatestCargoDeliveryDate_NoShipments() {
        // Arrange
        Long consoleId = 1L;
        ConsolidationDetails consolidationDetails = mock(ConsolidationDetails.class);
        when(consolidationDetails.getShipmentsList()).thenReturn(Collections.emptySet());
        when(consolidationDetailsDao.findById(consoleId)).thenReturn(Optional.of(consolidationDetails));

        // Act
        ResponseEntity<IRunnerResponse> response = shipmentService.getLatestCargoDeliveryDate(consoleId);

        // Assert
        assertNotNull(response);
        assertEquals(200, response.getStatusCodeValue());
    }

    @Test
    void sendEmailsForPushRequestAccept() throws Exception {
        ShipmentService spyService = spy(shipmentService);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails)));
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder().shipmentId(1L).consolidationId(2L).build();
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        spyService.sendEmailsForPushRequestAccept(testConsol, List.of(1L), new HashSet<>(), new ArrayList<>());
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void sendEmailsForPullRequestAccept() throws Exception {
        ShipmentService spyService = spy(shipmentService);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder().shipmentId(1L).consolidationId(2L).build();
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consolidationDetails)));
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        spyService.sendEmailsForPullRequestAccept(1L, 2L, new HashSet<>(), new ArrayList<>());
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void sendEmailForPushRequestWithdrawl() throws Exception {
        ShipmentService spyService = spy(shipmentService);
        when(consolidationDetailsDao.findConsolidationsByIds(any())).thenReturn(List.of(testConsol));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        spyService.sendEmailForPushRequestWithdrawl(1L, List.of(2L), new HashSet<>(), "rejectRemarks");
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullRequestWithdrawal() throws Exception {
        ShipmentService spyService = spy(shipmentService);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails));
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        spyService.sendEmailForPullRequestWithdrawal(consolidationDetails, List.of(2L), new HashSet<>(), "rejectRemarks");
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void sendEmailForPushRequestReject() throws Exception {
        ShipmentService spyService = spy(shipmentService);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails)));
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder().shipmentId(1L).consolidationId(2L).build();
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        spyService.sendEmailForPushRequestReject(consolidationDetails, List.of(2L), new HashSet<>(), "rejectRemarks", List.of(consoleShipmentMapping));
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullRequestReject() throws Exception {
        ShipmentService spyService = spy(shipmentService);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consolidationDetails)));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder().shipmentId(1L).consolidationId(2L).build();
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        spyService.sendEmailForPullRequestReject(1L, List.of(2L), new HashSet<>(), "rejectRemarks", List.of(consoleShipmentMapping));
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void sendEmailForPushRequested() throws Exception {
        ShipmentService spyService = spy(shipmentService);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        spyService.sendEmailForPushRequested(1L, 2L, new HashSet<>());
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void testCalculateShipmentSummary_success_emptyList() throws RunnerException {
        CalculateShipmentSummaryRequest calculateShipmentSummaryRequest = CalculateShipmentSummaryRequest.builder()
                .shipmentIdList(List.of())
                .build();

        var response = shipmentService.calculateShipmentSummary(CommonRequestModel.buildRequest(calculateShipmentSummaryRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testCalculateShipmentSummary_success_nullList() throws RunnerException {
        CalculateShipmentSummaryRequest calculateShipmentSummaryRequest = CalculateShipmentSummaryRequest.builder()
                .shipmentIdList(null)
                .build();

        var response = shipmentService.calculateShipmentSummary(CommonRequestModel.buildRequest(calculateShipmentSummaryRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testCalculateShipmentSummary_success() throws RunnerException {
        CalculateShipmentSummaryRequest calculateShipmentSummaryRequest = CalculateShipmentSummaryRequest.builder()
                .shipmentIdList(List.of(1L))
                .build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .transportMode(Constants.TRANSPORT_MODE_AIR)
                .noOfPacks(2)
                .packsUnit("BAG")
                .weight(BigDecimal.valueOf(123L))
                .weightUnit(Constants.WEIGHT_UNIT_KG)
                .volume(BigDecimal.valueOf(3L))
                .volumeUnit(Constants.VOLUME_UNIT_M3)
                .chargable(BigDecimal.valueOf(123L))
                .chargeableUnit(Constants.WEIGHT_UNIT_KG)
                .build();
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setWeightChargeableUnit(Constants.WEIGHT_UNIT_KG);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setVolumeChargeableUnit(Constants.VOLUME_UNIT_M3);


        mockTenantSettings();
        mockShipmentSettings();
        when(shipmentDao.findShipmentsByIds(Set.of(1L))).thenReturn(List.of(shipmentDetails));

        var response = shipmentService.calculateShipmentSummary(CommonRequestModel.buildRequest(calculateShipmentSummaryRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testIsConsoleCreationNeeded() {
        var mockRequest = new CustomerBookingRequest();
        mockRequest.setTransportType(Constants.TRANSPORT_MODE_SEA);
        mockRequest.setCargoType(Constants.CARGO_TYPE_FCL);
        var response = shipmentService.isConsoleCreationNeeded(mockRequest);
        assertTrue(response);
    }

    @Test
    void testIsConsoleCreationNeeded2() {
        var mockRequest = new CustomerBookingRequest();
        mockRequest.setTransportType(Constants.TRANSPORT_MODE_ROA);
        mockRequest.setCargoType(Constants.CARGO_TYPE_FCL);
        var response = shipmentService.isConsoleCreationNeeded(mockRequest);
        assertTrue(response);
    }

    @Test
    void testIsConsoleCreationNeeded3() {
        var mockRequest = new CustomerBookingRequest();
        mockRequest.setTransportType(Constants.TRANSPORT_MODE_ROA);
        mockRequest.setCargoType(Constants.CARGO_TYPE_FTL);
        var response = shipmentService.isConsoleCreationNeeded(mockRequest);
        assertTrue(response);
    }

    @Test
    void testIsConsoleCreationNeeded4() {
        var mockRequest = new CustomerBookingRequest();
        mockRequest.setTransportType(Constants.TRANSPORT_MODE_RAI);
        mockRequest.setCargoType(Constants.CARGO_TYPE_FCL);
        var response = shipmentService.isConsoleCreationNeeded(mockRequest);
        assertTrue(response);
    }

    @Test
    void testIsConsoleCreationNeeded5() {
        var mockRequest = new CustomerBookingRequest();
        mockRequest.setTransportType(Constants.TRANSPORT_MODE_SEA);
        mockRequest.setCargoType(Constants.SHIPMENT_TYPE_LCL);
        var response = shipmentService.isConsoleCreationNeeded(mockRequest);
        assertFalse(response);
    }

    @Test
    void testIsConsoleCreationNeeded6() {
        var mockRequest = new CustomerBookingRequest();
        mockRequest.setTransportType(Constants.TRANSPORT_MODE_ROA);
        mockRequest.setCargoType(Constants.SHIPMENT_TYPE_LCL);
        var response = shipmentService.isConsoleCreationNeeded(mockRequest);
        assertFalse(response);
    }

    @Test
    void testIsConsoleCreationNeeded7() {
        var mockRequest = new CustomerBookingRequest();
        mockRequest.setTransportType(Constants.TRANSPORT_MODE_RAI);
        mockRequest.setCargoType(Constants.SHIPMENT_TYPE_LCL);
        var response = shipmentService.isConsoleCreationNeeded(mockRequest);
        assertFalse(response);
    }

    @Test
    void partialTrackEventCreateEventsTest() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        EventsRequest eventsRequest = new EventsRequest();
        eventsRequest.setEventCode(EventConstants.EMCR);
        eventsRequest.setActual(LocalDateTime.now());
        eventsRequest.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).additionalDetail(AdditionalDetailRequest.builder().build()).build();
        shipmentPatchRequest.setEventsList(Arrays.asList(eventsRequest));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        Events event  = Events.builder().build().setEventCode(EventConstants.EMCR).setActual(LocalDateTime.now()).setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);

        ShipmentDetails oldshipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .additionalDetails(getmockAdditionalDetails(LocalDateTime.now(), false, false,false))
                .eventsList(Collections.singletonList(event))
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .shipmentType(Constants.CARGO_TYPE_FCL)
                .bookingNumber("1234-5678")
                .shipmentGateInDate(LocalDateTime.now())
                .build();
        LocalDateTime mockDateTimeNew = LocalDateTime.now().plusDays(2);
        AdditionalDetails additionalDetailsNew = getmockAdditionalDetails(mockDateTimeNew, true, true, true);

        ShipmentDetails newShipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .additionalDetails(additionalDetailsNew)
                .eventsList(Collections.singletonList(event))
                .transportMode(Constants.TRANSPORT_MODE_SEA)
                .shipmentType(Constants.CARGO_TYPE_FCL)
                .bookingNumber("5678-1234")
                .shipmentGateInDate(LocalDateTime.now().plusDays(1))
                .build();

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(oldshipmentDetails);
        when(jsonHelper.convertValue(any(), eq(AdditionalDetails.class))).thenReturn(additionalDetailsNew);
        when(additionalDetailDao.updateEntityFromShipment(any())).thenReturn(additionalDetailsNew);

        when(shipmentDao.findById(any())).thenReturn(Optional.of(newShipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(shipmentDao.update(any(), eq(false))).thenReturn(newShipmentDetails);
        when(jsonHelper.convertValueToList(any(), eq(Events.class))).thenReturn(Arrays.asList(event));
        mockTenantSettings();
        mockShipmentSettings();
        List<Events> eventsList = Arrays.asList(
                Events.builder().build().setEventCode(EventConstants.CURE).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER),
                Events.builder().build().setEventCode(EventConstants.CACO).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER),
                Events.builder().build().setEventCode(EventConstants.CADE).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER),
                Events.builder().build().setEventCode(EventConstants.DOTP).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER),
                Events.builder().build().setEventCode(EventConstants.PRDE).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER),
                Events.builder().build().setEventCode(EventConstants.SEPU).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER),
                Events.builder().build().setEventCode(EventConstants.CAFS).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER)
        );
        when(eventDao.updateEntityFromOtherEntity(any(), any(), any())).thenReturn(eventsList);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
        assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
        assertEquals(false, oldshipmentDetails.getAdditionalDetails().getDocTurnedOverToCustomer());
        assertEquals(true, newShipmentDetails.getAdditionalDetails().getDocTurnedOverToCustomer());
    }

    @Test
    void createShipmentEvents() throws RunnerException {
        UserContext.getUser().setPermissions(new HashMap<>());
        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(0L).build();
        ShipmentDetails mockShipment = shipmentDetails;
        shipmentDetails.setReceivingBranch(0L);
        shipmentDetails.setTriangulationPartnerList(List.of(triangulationPartner));
        shipmentDetails.setDocumentationPartner(0L);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.getAdditionalDetails().setDraftPrinted(true);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.getCarrierDetails().setAtd(LocalDateTime.now());
        shipmentDetails.setShipmentGateInDate(LocalDateTime.now());
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setBookingNumber("BookingNUmber");
        shipmentDetails.setDateType(DateBehaviorType.ESTIMATED);

        AdditionalDetails additionalDetails = getmockAdditionalDetails(LocalDateTime.now(), true, true, true);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        mockShipment.setShipmentId("AIR-CAN-00001");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(true).enableLclConsolidation(true).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.create(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void partialTrackEventUpdateEventsTest() throws RunnerException {
        List<Events> eventsList = Arrays.asList(
                Events.builder().build().setEventCode(EventConstants.CURE).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER).setActual(LocalDateTime.now()).setEstimated(LocalDateTime.now()),
                Events.builder().build().setEventCode(EventConstants.CACO).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER).setActual(LocalDateTime.now()).setEstimated(LocalDateTime.now()),
                Events.builder().build().setEventCode(EventConstants.CADE).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER).setActual(LocalDateTime.now()).setEstimated(LocalDateTime.now()),
                Events.builder().build().setEventCode(EventConstants.DOTP).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER).setActual(LocalDateTime.now()).setEstimated(LocalDateTime.now()),
                Events.builder().build().setEventCode(EventConstants.PRDE).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER).setActual(LocalDateTime.now()).setEstimated(LocalDateTime.now()),
                Events.builder().build().setEventCode(EventConstants.SEPU).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER).setActual(LocalDateTime.now()).setEstimated(LocalDateTime.now()),
                Events.builder().build().setEventCode(EventConstants.CAFS).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER).setActual(LocalDateTime.now()).setEstimated(LocalDateTime.now()),
                Events.builder().build().setEventCode(EventConstants.BOCO).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER).setActual(LocalDateTime.now()).setEstimated(LocalDateTime.now()),
                Events.builder().build().setEventCode(EventConstants.CAFS).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER).setActual(LocalDateTime.now()).setEstimated(LocalDateTime.now()),
                Events.builder().build().setEventCode(EventConstants.CAAW).setEntityType("SHIPMENT").setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER).setActual(LocalDateTime.now()).setEstimated(LocalDateTime.now())
        );
        mockShipmentSettings();
        for (Events event : eventsList) {
            ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
            EventsRequest eventsRequest = EventsRequest.builder().build();
            ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).additionalDetail(AdditionalDetailRequest.builder().build()).build();
            shipmentPatchRequest.setEventsList(Arrays.asList(eventsRequest));
            CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

            ShipmentDetails oldshipmentDetails = ShipmentDetails.builder()
                    .shipmentId("AIR-CAN-00001")
                    .shipmentCreatedOn(LocalDateTime.now())
                    .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                    .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                    .additionalDetails(getmockAdditionalDetails(LocalDateTime.now(), false, false, false))
                    .eventsList(Collections.singletonList(event))
                    .transportMode(Constants.TRANSPORT_MODE_SEA)
                    .shipmentType(Constants.SHIPMENT_TYPE_LCL)
                    .bookingNumber("1234-5678")
                    .shipmentGateInDate(LocalDateTime.now())
                    .dateType(DateBehaviorType.ACTUAL)
                    .build();
            LocalDateTime mockDateTimeNew = LocalDateTime.now().plusDays(2);
            AdditionalDetails additionalDetailsNew = getmockAdditionalDetails(mockDateTimeNew, true, true, true);

            ShipmentDetails newShipmentDetails = ShipmentDetails.builder()
                    .shipmentId("AIR-CAN-00001")
                    .shipmentCreatedOn(LocalDateTime.now())
                    .consolidationList(new HashSet<>(Arrays.asList(ConsolidationDetails.builder().build())))
                    .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                    .additionalDetails(additionalDetailsNew)
                    .eventsList(Collections.singletonList(event))
                    .transportMode(TRANSPORT_MODE_SEA)
                    .shipmentType(SHIPMENT_TYPE_LCL)
                    .bookingNumber("5678-1234")
                    .shipmentGateInDate(LocalDateTime.now().plusDays(1))
                    .dateType(DateBehaviorType.ACTUAL)
                    .carrierDetails(CarrierDetails.builder().build())
                    .build();

            when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(oldshipmentDetails);
            when(jsonHelper.convertValue(any(), eq(AdditionalDetails.class))).thenReturn(additionalDetailsNew);
            when(additionalDetailDao.updateEntityFromShipment(any())).thenReturn(additionalDetailsNew);

            when(shipmentDao.findById(any())).thenReturn(Optional.of(newShipmentDetails));
            doNothing().when(shipmentDetailsMapper).update(any(), any());
            when(shipmentDao.update(any(), eq(false))).thenReturn(newShipmentDetails);
            when(jsonHelper.convertValueToList(any(), eq(Events.class))).thenReturn(Arrays.asList(event));
            mockTenantSettings();
            when(eventDao.updateEntityFromOtherEntity(any(), any(), any())).thenReturn(eventsList);
            ResponseEntity<IRunnerResponse> httpResponse = shipmentService.partialUpdate(commonRequestModel, true);
            assertEquals(ResponseHelper.buildSuccessResponse(), httpResponse);
            assertEquals(false, oldshipmentDetails.getAdditionalDetails().getDocTurnedOverToCustomer());
            assertEquals(true, newShipmentDetails.getAdditionalDetails().getDocTurnedOverToCustomer());
        }
    }

    @Test
    void testSendOceanDGApprovalEmail_NullObject(){
        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentService.sendOceanDGApprovalEmail(null);
        });
    }

    @Test
    void testSendOceanDGApprovalEmail_shipmentNotFound(){
        OceanDGApprovalRequest request = OceanDGApprovalRequest
            .builder()
            .shipmentId(1l)
            .remarks("Shipment_Not_Found")
            .build();

        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentService.sendOceanDGApprovalEmail(request);
        });
    }

    @Test
    void testSendOceanDGApprovalEmail() throws RunnerException {
        try (MockedStatic<UserContext> userContextMockedStatic = Mockito.mockStatic(
            UserContext.class)) {
            OceanDGApprovalRequest request = OceanDGApprovalRequest
                .builder()
                .shipmentId(1l)
                .remarks("Non_DG_USER")
                .build();

            Packing packing = new Packing();
            packing.setHazardous(true);
            packing.setDGClass("1.2");
            ShipmentDetails shipmentDetails = ShipmentDetails
                .builder()
                .oceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED)
                .packingList(List.of(packing))
                .build();

            when(shipmentDao.findById(request.getShipmentId())).thenReturn(
                Optional.ofNullable(shipmentDetails));


            UsersDto user = UsersDto.builder().build();
            userContextMockedStatic.when(UserContext::getUser).thenReturn(user);
            userContextMockedStatic.when(UserContext::isOceanDgUser).thenReturn(false);

            when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
            Integer roleId = 1;
            List<String> users = new ArrayList<>();
            users.add("abc@email.com");
            TaskCreateResponse taskCreateResponse = TaskCreateResponse.builder().build();
            when(commonUtils.getRoleId(any())).thenReturn(roleId);
            when(commonUtils.getUserEmailsByRoleId(roleId)).thenReturn(users);
            when(commonUtils.createTask(shipmentDetails, roleId)).thenReturn(taskCreateResponse);

            assertThrows(RunnerException.class,() ->shipmentService.sendOceanDGApprovalEmail(request));
        }
    }


    @Test
    void testSendOceanDGApprovalEmail_Commercial() throws RunnerException {
        try (MockedStatic<UserContext> userContextMockedStatic = Mockito.mockStatic(
            UserContext.class)) {
            OceanDGApprovalRequest request = OceanDGApprovalRequest
                .builder()
                .shipmentId(1l)
                .remarks("Non_DG_USER")
                .build();

            ShipmentDetails shipmentDetails = ShipmentDetails
                .builder()
                .oceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED)
                .containersList(Set.of(Containers.builder().hazardous(true).dgClass("1.2").build()))
                .build();

            when(shipmentDao.findById(request.getShipmentId())).thenReturn(
                Optional.ofNullable(shipmentDetails));


            UsersDto user = UsersDto.builder().build();
            userContextMockedStatic.when(UserContext::getUser).thenReturn(user);
            userContextMockedStatic.when(UserContext::isOceanDgUser).thenReturn(false);

            when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
            Integer roleId = 1;
            List<String> users = new ArrayList<>();
            users.add("abc@email.com");
            TaskCreateResponse taskCreateResponse = TaskCreateResponse.builder().build();
            when(commonUtils.getRoleId(any())).thenReturn(roleId);
            when(commonUtils.getUserEmailsByRoleId(roleId)).thenReturn(users);
            when(commonUtils.createTask(shipmentDetails, roleId)).thenReturn(taskCreateResponse);

            assertThrows(RunnerException.class,()-> shipmentService.sendOceanDGApprovalEmail(request));
            verify(shipmentDao).findById(any());
        }
    }

    @Test
    void testSendOceanDGApprovalEmail_DgUser() throws RunnerException {
        try (MockedStatic<UserContext> userContextMockedStatic = Mockito.mockStatic(
            UserContext.class)) {
            OceanDGApprovalRequest request = OceanDGApprovalRequest
                .builder()
                .shipmentId(1l)
                .remarks("Non_DG_USER")
                .build();

            Packing packing = new Packing();
            packing.setHazardous(true);
            packing.setDGClass("1.23");

            ShipmentDetails shipmentDetails = ShipmentDetails
                .builder()
                .oceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED)
                .containersList(Set.of(Containers.builder().hazardous(true).dgClass("2.1").build()))
                .packingList(List.of(packing))
                .build();

            when(shipmentDao.findById(request.getShipmentId())).thenReturn(
                Optional.ofNullable(shipmentDetails));

            UsersDto user = UsersDto.builder().build();
            userContextMockedStatic.when(UserContext::getUser).thenReturn(user);
            userContextMockedStatic.when(UserContext::isOceanDgUser).thenReturn(true);

           shipmentService.sendOceanDGApprovalEmail(request);
            verify(shipmentDao).findById(any());
        }
    }

    @Test
    void testSendOceanDGApprovalEmail_Imp() throws RunnerException {
        OceanDGApprovalRequest request = OceanDGApprovalRequest
                .builder()
                .shipmentId(1l)
                .remarks("")
                .build();
        ShipmentDetails shipmentDetails = ShipmentDetails
                .builder()
                .oceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED)
                .containersList(Set.of(Containers.builder().hazardous(true).dgClass("2.1").build()))
                .direction(Constants.IMP)
                .build();

        when(shipmentDao.findById(request.getShipmentId())).thenReturn(
                Optional.ofNullable(shipmentDetails));
        shipmentService.sendOceanDGApprovalEmail(request);
        verify(shipmentDao).findById(any());
    }

    @Test
    void testDgApprovalResponse_Imp() throws RunnerException {
        OceanDGRequest request = OceanDGRequest
                .builder()
                .shipmentId(1l)
                .remarks("")
                .build();
        ShipmentDetails shipmentDetails = ShipmentDetails
                .builder()
                .oceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED)
                .containersList(Set.of(Containers.builder().hazardous(true).dgClass("2.1").build()))
                .direction(Constants.IMP)
                .build();

        when(shipmentDao.findById(request.getShipmentId())).thenReturn(
                Optional.ofNullable(shipmentDetails));
        shipmentService.dgApprovalResponse(request);
        verify(shipmentDao).findById(any());
    }

    @Test
    void testDgApprovalResponse_NullRequest(){
        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentService.dgApprovalResponse(null);
        });
    }

    @Test
    void testDgApprovalResponse_ShipmentNotFound(){
        OceanDGRequest request = OceanDGRequest.builder().shipmentId(1l).build();

        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentService.dgApprovalResponse(request);
        });
    }

    @Test
    void testDgApprovalResponse_InvalidDGStatus(){
        OceanDGRequest request = OceanDGRequest.builder().shipmentId(1l).build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().oceanDGStatus(OceanDGStatus.APPROVE).build();
        when(shipmentDao.findById(request.getShipmentId())).thenReturn(
            Optional.ofNullable(shipmentDetails));

        assertThrows(RunnerException.class, () -> {
            shipmentService.dgApprovalResponse(request);
        });
        verify(shipmentDao).findById(any());
    }

    @Test
    void testDgApprovalResponse_ValidDgApprove() throws RunnerException {
        OceanDGRequest request = OceanDGRequest.builder().shipmentId(1l).status(TaskStatus.APPROVED).build();

        Packing packing = new Packing();
        packing.setHazardous(true);
        packing.setDGClass("1.23");

        ShipmentDetails shipmentDetails = ShipmentDetails
            .builder()
            .oceanDGStatus(OceanDGStatus.OCEAN_DG_REQUESTED)
            .containersList(Set.of(Containers.builder().hazardous(true).dgClass("2.1").build()))
            .packingList(List.of(packing))
            .build();
        when(shipmentDao.findById(request.getShipmentId())).thenReturn(
            Optional.ofNullable(shipmentDetails));


        V1DataResponse v1Response = V1DataResponse.builder().build();
        when(v1Service.listTask(any())).thenReturn(v1Response);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());

        List<TaskCreateRequest> taskCreateRequestList = new ArrayList<>();
        taskCreateRequestList.add(TaskCreateRequest.builder().id("12").userEmail("hc@email.com").build());

        when(jsonHelper.convertValueToList(v1Response.getEntities(), TaskCreateRequest.class)).thenReturn(taskCreateRequestList);

        shipmentService.dgApprovalResponse(request);
        verify(v1Service).listTask(any());
    }

    @Test
    void testSendEmailForApproval_DG() throws RunnerException {
        Map<OceanDGStatus, EmailTemplatesRequest> emailTemplatesRequestMap = new HashMap<>();
        String body = "<table border=\"1\" cellpadding=\"1\" cellspacing=\"1\" style=\"width:500px\">\n"
            + "    <tbody>\n"
            + "        <tr>\n"
            + "            <td>&nbsp;No &amp; Type of Package&nbsp;</td>\n"
            + "            <td>Container Number</td>\n"
            + "            <td>&nbsp;DG Class</td>\n"
            + "            <td>UN Number</td>\n"
            + "            <td>Proper Shipping Name</td>\n"
            + "            <td>Packing Group&nbsp;</td>\n"
            + "            <td>Minimum Flash Point</td>\n"
            + "            <td>Marine Pollutant</td>\n"
            + "        </tr>\n"
            + "        <tr>\n"
            + "            <td>#PACKAGE_DETAILS}</td>\n"
            + "            <td>{#CONTAINER_NUMBER}&nbsp;</td>\n"
            + "            <td>{#DG_CLASS}</td>\n"
            + "            <td>{#UN_NUMBER}</td>\n"
            + "            <td>{#SHIPPING_NAME}</td>\n"
            + "            <td>{#PACKING_GROUP}</td>\n"
            + "            <td>{#FLASH_POINT}&nbsp;</td>\n"
            + "            <td>{#MARINE_POLLUTANT}</td>\n"
            + "        </tr>\n"
            + "    </tbody>\n"
            + "</table>";
        EmailTemplatesRequest emailTemplatesRequest = EmailTemplatesRequest.builder().body(body).build();
        emailTemplatesRequestMap.put(OceanDGStatus.OCEAN_DG_REQUESTED, emailTemplatesRequest);

        List<String> toEmailIds = List.of("hc@gmail.com");
        VesselsResponse vesselsResponse = new VesselsResponse();
        OceanDGStatus templateStatus = OceanDGStatus.OCEAN_DG_REQUESTED;

        Containers containers1 = Containers.builder()
            .containerNumber("CC!2").dgClass("1.2").unNumber("12")
            .properShippingName("213").packingGroup("42")
            .marinePollutant(true).minimumFlashPoint(BigDecimal.valueOf(12))
            .hazardous(true)
            .build();
        containers1.setId(1l);

        Containers containers2 = Containers.builder().build();
        containers2.setId(2l);

        Packing packing1 = new Packing();
        packing1.setHazardous(true);
        packing1.setDGClass("1.2");
        packing1.setContainerId(1l);
        packing1.setUnNumber("12");
        packing1.setProperShippingName("12");
        packing1.setPackingGroup("12");
        packing1.setMarinePollutant(true);
        packing1.setMinimumFlashPoint(BigDecimal.valueOf(33.0));

        Packing packing2 = new Packing();
        packing2.setHazardous(true);

        TaskCreateResponse taskCreateResponse = TaskCreateResponse.builder().build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
            .containersList(new HashSet<>(List.of(containers1, containers2)))
            .packingList(List.of(packing1, packing2))
            .build();
        String remarks = "Remarks";

        shipmentService.sendEmailForApproval(emailTemplatesRequestMap, toEmailIds, vesselsResponse, templateStatus, shipmentDetails,
            remarks, taskCreateResponse);
        assertEquals("Remarks", remarks);
    }

    @Test
    void testSendEmailForApproval_Commercial() throws RunnerException {
        Map<OceanDGStatus, EmailTemplatesRequest> emailTemplatesRequestMap = new HashMap<>();
        EmailTemplatesRequest emailTemplatesRequest = EmailTemplatesRequest.builder().body("<>").build();
        emailTemplatesRequestMap.put(OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED, emailTemplatesRequest);

        List<String> toEmailIds = List.of("hc@gmail.com");
        VesselsResponse vesselsResponse = new VesselsResponse();
        OceanDGStatus templateStatus = OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED;

        TaskCreateResponse taskCreateResponse = TaskCreateResponse.builder().build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        String remarks = "Remarks";

        shipmentService.sendEmailForApproval(emailTemplatesRequestMap, toEmailIds, vesselsResponse, templateStatus, shipmentDetails,
            remarks, taskCreateResponse);
        assertEquals("Remarks", remarks);
    }

    @Test
    void testOceanDGValidations_Error() throws RunnerException {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        consolidationDetails1.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        consolidationDetails1.setHazardous(true);
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(List.of(ConsoleShipmentMapping.builder().build()));
        assertThrows(RunnerException.class, () -> shipmentService.dgValidations(shipmentDetails1, consolidationDetails1, 1));
    }

    @Test
    void testOceanDGValidations() throws RunnerException {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        consolidationDetails1.setContainerCategory(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setContainsHazardous(true);
        mockShipmentSettings();
        shipmentService.dgValidations(shipmentDetails1, consolidationDetails1, 0);
        verify(consoleShipmentMappingDao, times(0)).findByConsolidationId(any());
    }

    @Test
    void testOceanDGValidations1() throws RunnerException {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        consolidationDetails1.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        shipmentService.dgValidations(shipmentDetails1, consolidationDetails1, 0);
        verify(consoleShipmentMappingDao, times(0)).findByConsolidationId(any());
    }

    @Test
    void testOceanDGValidations2() throws RunnerException {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        consolidationDetails1.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        consolidationDetails1.setHazardous(true);
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(new ArrayList<>());
        shipmentService.dgValidations(shipmentDetails1, consolidationDetails1, 0);
        verify(consoleShipmentMappingDao, times(1)).findByConsolidationId(any());
    }

    @Test
    void testAirDGValidations() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setContainsHazardous(true);
        assertThrows(RunnerException.class, () -> shipmentService.airDGValidations(shipmentDetails1, null, null, new MutableBoolean(true), null));
    }

    @Test
    void testAirDGValidations1() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setContainsHazardous(true);
        assertThrows(RunnerException.class, () -> shipmentService.airDGValidations(shipmentDetails1, null, List.of(1L), new MutableBoolean(false), null));
    }

    @Test
    void testAirDGValidations3() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setContainsHazardous(true);
        shipmentService.airDGValidations(shipmentDetails1, null, null, new MutableBoolean(false), null);
        verify(consoleShipmentMappingDao, times(0)).findByConsolidationId(any());
    }

    @Test
    void testAirDGValidations4() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentService.airDGValidations(shipmentDetails1, null, null, null, null);
        verify(consoleShipmentMappingDao, times(0)).findByConsolidationId(any());
    }

    @Test
    void testAirDGValidations5() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentService.airDGValidations(shipmentDetails1, null, List.of(2L), null, Set.of(new ConsolidationDetailsRequest()));
        verify(consoleShipmentMappingDao, times(0)).findByConsolidationId(any());
    }

    @Test
    void testAirDGValidations6() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ConsolidationDetailsRequest consolidationDetailsRequest = new ConsolidationDetailsRequest();
        consolidationDetailsRequest.setHazardous(true);
        assertThrows(RunnerException.class, () -> shipmentService.airDGValidations(shipmentDetails1, new ShipmentDetails(), List.of(2L), null, Set.of(consolidationDetailsRequest)));
    }

    @Test
    void testAirDGValidations7() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setConsolidationList(Set.of(new ConsolidationDetails()));
        shipmentService.airDGValidations(shipmentDetails1, new ShipmentDetails(), List.of(2L), null, null);
        verify(consoleShipmentMappingDao, times(0)).findByConsolidationId(any());
    }

    @Test
    void testAirDGValidations8() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setHazardous(true);
        shipmentDetails1.setConsolidationList(Set.of(consolidationDetails1));
        assertThrows(RunnerException.class, () -> shipmentService.airDGValidations(shipmentDetails1, shipmentDetails1, List.of(2L), null, null));
    }

    @Test
    void testMakeShipmentsDG() throws RunnerException {
        shipmentService.makeShipmentsDG(new HashMap<>(), ShipmentDetails.builder().containsHazardous(true).build());
        verify(shipmentDao, times(0)).save(any(), anyBoolean());
    }

    @Test
    void testMakeShipmentsDG2() throws RunnerException {
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().build();
        shipmentDetails1.setGuid(UUID.randomUUID());
        when(commonUtils.checkIfDGClass1(any())).thenReturn(true);
        doNothing().when(shipmentDao).entityDetach(any());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(ShipmentDetails.builder().oceanDGStatus(OceanDGStatus.OCEAN_DG_ACCEPTED).build()));
        when(shipmentDao.save(any(), anyBoolean())).thenReturn(shipmentDetails1);
        shipmentService.makeShipmentsDG(new HashMap<>() {{
            put(1L, Containers.builder().hazardous(true).dgClass("1.1").build());
        }}, shipmentDetails1);
        verify(shipmentDao, times(1)).save(any(), anyBoolean());
    }

    @Test
    void testMakeShipmentsDG3() throws RunnerException {
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().build();
        shipmentDetails1.setGuid(UUID.randomUUID());
        when(commonUtils.checkIfDGClass1(any())).thenReturn(true);
        doNothing().when(shipmentDao).entityDetach(any());
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
        shipmentService.makeShipmentsDG(new HashMap<>() {{
            put(1L, Containers.builder().hazardous(true).dgClass("1.1").build());
        }}, shipmentDetails1);
        verify(shipmentDao, times(0)).save(any(), anyBoolean());
    }

    @Test
    void testMakeShipmentsDG4() throws RunnerException {
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().build();
        shipmentDetails1.setGuid(UUID.randomUUID());
        when(commonUtils.checkIfDGClass1(any())).thenReturn(false);
        doNothing().when(shipmentDao).entityDetach(any());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(ShipmentDetails.builder().oceanDGStatus(OceanDGStatus.OCEAN_DG_ACCEPTED).build()));
        when(shipmentDao.save(any(), anyBoolean())).thenReturn(shipmentDetails1);
        shipmentService.makeShipmentsDG(new HashMap<>() {{
            put(1L, Containers.builder().hazardous(true).build());
        }}, shipmentDetails1);
        verify(shipmentDao, times(1)).save(any(), anyBoolean());
    }

    @Test
    void testMakeShipmentsDG5() throws RunnerException {
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().build();
        shipmentDetails1.setGuid(UUID.randomUUID());
        when(commonUtils.checkIfDGClass1(any())).thenReturn(true);
        doNothing().when(shipmentDao).entityDetach(any());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(ShipmentDetails.builder().build()));
        when(shipmentDao.save(any(), anyBoolean())).thenReturn(shipmentDetails1);
        shipmentService.makeShipmentsDG(new HashMap<>() {{
            put(1L, Containers.builder().hazardous(true).dgClass("1.1").build());
        }}, shipmentDetails1);
        verify(shipmentDao, times(1)).save(any(), anyBoolean());
    }

    @Test
    void testIsOceanDG() throws RunnerException {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
            .containersList(Set.of(Containers.builder().build()))
            .packingList(List.of(new Packing()))
            .build();

        OceanDGApprovalRequest request = OceanDGApprovalRequest
            .builder()
            .shipmentId(1l)
            .remarks("Non_DG_USER")
            .build();

        when(shipmentDao.findById(request.getShipmentId())).thenReturn(
            Optional.ofNullable(shipmentDetails));

        shipmentService.sendOceanDGApprovalEmail(request);
        verify(shipmentDao).findById(any());
    }


    @Test
    void testIsOceanDG_False() throws RunnerException {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
            .containersList(Set.of(Containers.builder().hazardous(true).dgClass("2.3").build()))
            .packingList(List.of(new Packing()))
            .oceanDGStatus(OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED)
            .build();

        OceanDGApprovalRequest request = OceanDGApprovalRequest
            .builder()
            .shipmentId(1l)
            .remarks("Non_DG_USER")
            .build();

        when(shipmentDao.findById(request.getShipmentId())).thenReturn(
            Optional.ofNullable(shipmentDetails));

        shipmentService.sendOceanDGApprovalEmail(request);
        verify(shipmentDao).findById(any());
    }

    @Test
    void listWithoutTenantCheckCatch() {
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.listWithoutTenantCheck(CommonRequestModel.builder().build());
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listRequestWithoutTenantCheckNull() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.listWithoutTenantCheck(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listWithoutTenantCheck() {
        Criteria criteria = Criteria.builder().fieldName("shipmentId").value("1").build();
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(Arrays.asList(FilterCriteria.builder().criteria(criteria).innerFilter(Arrays.asList(FilterCriteria.builder().build())).build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAllWithoutTenantFilter(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityToDtoListSimplified(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.listWithoutTenantCheck(commonRequestModel);
        assertEquals(expectedResponse, httpResponse);
    }

    @Test
    void listWithoutTenantCheckPartial() {
        Criteria criteria = Criteria.builder().fieldName("shipmentId").value("1").build();
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(Arrays.asList(FilterCriteria.builder().criteria(criteria).innerFilter(Arrays.asList(FilterCriteria.builder().build())).build())).includeColumns(Arrays.asList("shipmentId")).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAllWithoutTenantFilter(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.listWithoutTenantCheck(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void createShipmentFromBookingTest() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        Parties importBroker = Parties.builder().orgCode("1223").build();
        PartiesRequest importBrokerRequest = PartiesRequest.builder().build();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setImportBroker(importBroker);
        additionalDetails.setExportBroker(importBroker);
        AdditionalDetailRequest additionalDetailsRequest = new AdditionalDetailRequest();
        additionalDetailsRequest.setImportBroker(importBrokerRequest);
        additionalDetailsRequest.setExportBroker(importBrokerRequest);

        ContainerRequest containerRequest = ContainerRequest.builder().build();

        ConsolidationDetailsRequest consolidationDetailsRequest = ConsolidationDetailsRequest.builder().build();
        consolidationDetailsRequest.setReferenceNumber("1");
        consolidationDetailsRequest.setBol("1");
        consolidationDetailsRequest.setContainersList(Collections.singletonList(containerRequest));

        RoutingsRequest routingsRequest = RoutingsRequest.builder().build();
        NotesRequest notesRequest = NotesRequest.builder().build();
        PackingRequest packingRequest = PackingRequest.builder().build();

        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).shipmentType(Constants.CARGO_TYPE_FCL).carrierDetails(CarrierDetailRequest.builder().build()).orderManagementId("eaf227f3-de85-42b4-8180-cf48ccf568f9").build();
        shipmentRequest.setAdditionalDetails(additionalDetailsRequest);
        shipmentRequest.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetailsRequest)));
        shipmentRequest.setRoutingsList(Collections.singletonList(routingsRequest));
        shipmentRequest.setNotesList(Collections.singletonList(notesRequest));
        shipmentRequest.setPackingList(Collections.singletonList(packingRequest));

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").build().setReferenceNumbersList(Collections.singletonList(referenceNumbers)).setAdditionalDetails(additionalDetails).setGoodsDescription("Abcd").setTransportMode("FCL");
        shipmentDetails.setGuid(UUID.randomUUID());

        when(jsonHelper.convertValueToList(any(), eq(ConsolidationDetails.class))).thenReturn(Collections.singletonList(new ConsolidationDetails()));
        when(jsonHelper.convertValueToList(any(), eq(Containers.class))).thenReturn(Collections.singletonList(new Containers()));
        when(jsonHelper.convertValueToList(any(), eq(Routings.class))).thenReturn(Collections.singletonList(new Routings()));
        when(jsonHelper.convertValueToList(any(), eq(Packing.class))).thenReturn(Collections.singletonList(new Packing()));

        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolRequest.class))).thenReturn(new AutoUpdateWtVolRequest());
        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolResponse.class))).thenReturn(new AutoUpdateWtVolResponse());
        when(jsonHelper.convertValue(any(), eq(Notes.class))).thenReturn(new Notes());

        ReferenceNumbersRequest referenceNumberObj2 = ReferenceNumbersRequest.builder().build();

        when(jsonHelper.convertValue(shipmentDetails.getAdditionalDetails().getImportBroker(), PartiesRequest.class)).thenReturn(importBrokerRequest);
        when(jsonHelper.convertValue(shipmentDetails.getAdditionalDetails().getExportBroker(), PartiesRequest.class)).thenReturn(importBrokerRequest);
        when(jsonHelper.convertValue(anyList(), any(TypeReference.class))).thenReturn(Collections.singletonList(referenceNumberObj2));

        when(jsonHelper.convertValueToList(any(), eq(ReferenceNumbers.class))).thenReturn(Collections.singletonList(new ReferenceNumbers()));
        when(referenceNumbersDao.saveEntityFromShipment(any(), any())).thenReturn(Collections.singletonList(referenceNumbers));

        when(jsonHelper.convertValueToList(any(), eq(ShipmentOrder.class))).thenReturn(Collections.singletonList(new ShipmentOrder()));
        when(shipmentOrderDao.updateEntityFromShipment(any(), any())).thenReturn(Collections.singletonList(new ShipmentOrder()));
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails);

        ConsolidationDetailsResponse consolidationDetailsResponse = ConsolidationDetailsResponse.builder().build();
        when(consolidationService.createConsolidationForBooking(any())).thenReturn(consolidationDetailsResponse);
        when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);

        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().build();
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(shipmentDetailsResponse);

        when(jsonHelper.convertToJson(any())).thenReturn("shipmentDetailsString");
        mockShipmentSettings();
        mockTenantSettings();

        when(orderManagementAdapter.getOrderByGuid(any())).thenReturn(shipmentDetails);
        String shipmentResponse = shipmentService.createShipmentFromBooking(shipmentRequest);
        assertNotNull(shipmentResponse);
    }

    @Test
    void createShipmentFromBookingTest2() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ConsolidationDetailsRequest consolidationDetailsRequest = ConsolidationDetailsRequest.builder().build();
        consolidationDetailsRequest.setReferenceNumber("1");
        consolidationDetailsRequest.setBol("1");
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).shipmentType(Constants.CARGO_TYPE_FCL).carrierDetails(CarrierDetailRequest.builder().build()).orderManagementId("eaf227f3-de85-42b4-8180-cf48ccf568f9").build();
        shipmentRequest.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetailsRequest)));

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").build().setGoodsDescription("Abcd").setTransportMode("FCL");
        shipmentDetails.setGuid(UUID.randomUUID());

        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolRequest.class))).thenReturn(new AutoUpdateWtVolRequest());
        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolResponse.class))).thenReturn(new AutoUpdateWtVolResponse());

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails);

        ConsolidationDetailsResponse consolidationDetailsResponse = ConsolidationDetailsResponse.builder().build();
        when(consolidationService.createConsolidationForBooking(any())).thenReturn(consolidationDetailsResponse);
        when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);

        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().build();
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(shipmentDetailsResponse);

        when(jsonHelper.convertToJson(any())).thenReturn("shipmentDetailsString");
        mockShipmentSettings();
        mockTenantSettings();

        when(orderManagementAdapter.getOrderByGuid(any())).thenReturn(shipmentDetails);
        String shipmentResponse = shipmentService.createShipmentFromBooking(shipmentRequest);
        assertNotNull(shipmentResponse);
    }

    @Test
    void createShipmentFromBookingTest3() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).shipmentType(Constants.CARGO_TYPE_FCL).carrierDetails(CarrierDetailRequest.builder().build()).orderManagementId("eaf227f3-de85-42b4-8180-cf48ccf568f9").build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").build().setGoodsDescription("Abcd").setTransportMode("FCL");
        shipmentDetails.setGuid(UUID.randomUUID());

        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolRequest.class))).thenReturn(new AutoUpdateWtVolRequest());
        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolResponse.class))).thenReturn(new AutoUpdateWtVolResponse());

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails);

        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().build();
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(shipmentDetailsResponse);

        when(jsonHelper.convertToJson(any())).thenReturn("shipmentDetailsString");
        mockShipmentSettings();
        mockTenantSettings();

        when(orderManagementAdapter.getOrderByGuid(any())).thenReturn(null);
        String shipmentResponse = shipmentService.createShipmentFromBooking(shipmentRequest);
        assertNotNull(shipmentResponse);
    }

    @Test
    void createShipmentFromBookingTest4() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        AdditionalDetailRequest additionalDetailsRequest = new AdditionalDetailRequest();

        ConsolidationDetailsRequest consolidationDetailsRequest = ConsolidationDetailsRequest.builder().build();
        consolidationDetailsRequest.setReferenceNumber("1");
        consolidationDetailsRequest.setBol("1");
        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).shipmentType(Constants.CARGO_TYPE_FCL).carrierDetails(CarrierDetailRequest.builder().build()).orderManagementId("eaf227f3-de85-42b4-8180-cf48ccf568f9").build();
        shipmentRequest.setAdditionalDetails(additionalDetailsRequest);
        shipmentRequest.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetailsRequest)));

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").build().setReferenceNumbersList(Collections.singletonList(referenceNumbers)).setAdditionalDetails(additionalDetails).setTransportMode("FCL");
        shipmentDetails.setGuid(UUID.randomUUID());

        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolRequest.class))).thenReturn(new AutoUpdateWtVolRequest());
        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolResponse.class))).thenReturn(new AutoUpdateWtVolResponse());

        ReferenceNumbersRequest referenceNumberObj2 = ReferenceNumbersRequest.builder().build();

        when(jsonHelper.convertValue(anyList(), any(TypeReference.class))).thenReturn(Collections.singletonList(referenceNumberObj2));

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails);

        ConsolidationDetailsResponse consolidationDetailsResponse = ConsolidationDetailsResponse.builder().build();
        when(consolidationService.createConsolidationForBooking(any())).thenReturn(consolidationDetailsResponse);
        when(jsonHelper.convertValue(any(), eq(ConsolidationDetailsRequest.class))).thenReturn(consolidationDetailsRequest);

        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().build();
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(shipmentDetailsResponse);

        when(jsonHelper.convertToJson(any())).thenReturn("shipmentDetailsString");
        mockShipmentSettings();
        mockTenantSettings();

        when(orderManagementAdapter.getOrderByGuid(any())).thenReturn(shipmentDetails);
        String shipmentResponse = shipmentService.createShipmentFromBooking(shipmentRequest);
        assertNotNull(shipmentResponse);
    }

    @Test
    void createShipmentFromBookingTest5() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest shipmentRequest = ShipmentRequest.builder().id(1L).shipmentType(Constants.CARGO_TYPE_FCL).carrierDetails(CarrierDetailRequest.builder().build()).build();

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").build().setTransportMode("FCL");
        shipmentDetails.setGuid(UUID.randomUUID());

        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolRequest.class))).thenReturn(new AutoUpdateWtVolRequest());
        when(jsonHelper.convertValue(any(), eq(AutoUpdateWtVolResponse.class))).thenReturn(new AutoUpdateWtVolResponse());

        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(shipmentDao.save(any(), eq(false))).thenReturn(shipmentDetails);

        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().build();
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(shipmentDetailsResponse);

        when(jsonHelper.convertToJson(any())).thenReturn("shipmentDetailsString");
        mockShipmentSettings();
        mockTenantSettings();

        String shipmentResponse = shipmentService.createShipmentFromBooking(shipmentRequest);
        assertNotNull(shipmentResponse);
    }

    @Test
    void completeUpdateShipmentOrderTest() throws RunnerException {
        shipmentDetails.setId(1L);
        List<ShipmentOrder> shipmentOrderList = Collections.singletonList(ShipmentOrder.builder().build());
        ShipmentDetails mockShipment = objectMapper.convertValue(shipmentDetails, ShipmentDetails.class);
        mockShipment.setShipmentOrders(shipmentOrderList);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        // Mock
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails.setConsolidationList(new HashSet<>())
                                        .setContainersList(new HashSet<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(jsonHelper.convertValueToList(any(), eq(RoutingsRequest.class))).thenReturn(List.of(new RoutingsRequest()));
        when(jsonHelper.convertValueToList(any(), eq(ShipmentOrder.class))).thenReturn(shipmentOrderList);
        when(shipmentOrderDao.updateEntityFromShipment(any(), any())).thenReturn(shipmentOrderList);
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(this::mockRunnable);
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        mockShipmentSettings();
        mockTenantSettings();
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void attachDetachOrderTest() {
        ShipmentOrderAttachDetachRequest shipmentRequest = ShipmentOrderAttachDetachRequest.builder().build();
        shipmentRequest.setOrderDetailsList(List.of(ShipmentOrderAttachDetachRequest.OrderDetails.builder().orderGuid(UUID.fromString("eaf227f3-de85-42b4-8180-cf48ccf568f9")).build()));
        shipmentRequest.setShipmentGuid(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06"));
        shipmentRequest.setEvent("ATTACH");
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().build();
        shipmentDetails1.setId(1L);

        ShipmentOrder shipmentOrder = ShipmentOrder.builder().build();
        shipmentOrder.setId(1L);
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails1));
        when(shipmentOrderDao.findByShipmentId(any())).thenReturn(List.of(shipmentOrder));

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.attachDetachOrder(shipmentRequest);
        assertEquals(httpResponse, ResponseHelper.buildSuccessResponse());
    }

    @Test
    void attachDetachOrderTest2() {
        ShipmentOrderAttachDetachRequest shipmentRequest = ShipmentOrderAttachDetachRequest.builder().build();
        shipmentRequest.setOrderDetailsList(List.of(ShipmentOrderAttachDetachRequest.OrderDetails.builder().orderGuid(UUID.fromString("eaf227f3-de85-42b4-8180-cf48ccf568f9")).build()));
        shipmentRequest.setShipmentGuid(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06"));
        shipmentRequest.setEvent("ATTACH");
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().build();
        shipmentDetails1.setId(1L);

        ShipmentOrder shipmentOrder = ShipmentOrder.builder().build();
        shipmentOrder.setId(1L);
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails1));
        when(shipmentOrderDao.findByShipmentId(any())).thenReturn(new ArrayList<>());
        when(shipmentOrderDao.save(any())).thenReturn(shipmentOrder);

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.attachDetachOrder(shipmentRequest);
        assertEquals(httpResponse, ResponseHelper.buildSuccessResponse());
    }

    @Test
    void attachDetachOrderTest3() {
        ShipmentOrderAttachDetachRequest shipmentRequest = ShipmentOrderAttachDetachRequest.builder().build();
        shipmentRequest.setOrderDetailsList(List.of(ShipmentOrderAttachDetachRequest.OrderDetails.builder().orderGuid(UUID.fromString("eaf227f3-de85-42b4-8180-cf48ccf568f9")).build()));
        shipmentRequest.setShipmentGuid(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06"));
        shipmentRequest.setEvent("DETACH");
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().build();
        shipmentDetails1.setId(1L);

        ShipmentOrder shipmentOrder = ShipmentOrder.builder().build();
        shipmentOrder.setId(1L);
        shipmentOrder.setOrderGuid(UUID.fromString("eaf227f3-de85-42b4-8180-cf48ccf568f9"));
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails1));
        when(shipmentOrderDao.findByShipmentId(any())).thenReturn(List.of(shipmentOrder));
        doNothing().when(shipmentOrderDao).delete(any());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.attachDetachOrder(shipmentRequest);
        assertEquals(httpResponse, ResponseHelper.buildSuccessResponse());
    }

    @Test
    void attachDetachOrderTest4() {
        ShipmentOrderAttachDetachRequest shipmentRequest = ShipmentOrderAttachDetachRequest.builder().build();
        shipmentRequest.setOrderDetailsList(List.of(ShipmentOrderAttachDetachRequest.OrderDetails.builder().orderGuid(UUID.fromString("eaf227f3-de85-42b4-8180-cf48ccf568f9")).build()));
        shipmentRequest.setShipmentGuid(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06"));
        shipmentRequest.setEvent("DETACH");
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().build();
        shipmentDetails1.setId(1L);

        ShipmentOrder shipmentOrder = ShipmentOrder.builder().build();
        shipmentOrder.setId(1L);
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails1));
        when(shipmentOrderDao.findByShipmentId(any())).thenReturn(new ArrayList<>());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.attachDetachOrder(shipmentRequest);
        assertEquals(httpResponse, ResponseHelper.buildSuccessResponse());
    }


    @Test
    void attachDetachOrderTest5_ThrowException() {
        ShipmentOrderAttachDetachRequest shipmentRequest = ShipmentOrderAttachDetachRequest.builder().build();
        shipmentRequest.setOrderDetailsList(List.of(ShipmentOrderAttachDetachRequest.OrderDetails.builder().orderGuid(UUID.fromString("eaf227f3-de85-42b4-8180-cf48ccf568f9")).build()));
        shipmentRequest.setShipmentGuid(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06"));
        shipmentRequest.setEvent("DETACHED");
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().build();
        shipmentDetails1.setId(1L);

        ShipmentOrder shipmentOrder = ShipmentOrder.builder().build();
        shipmentOrder.setId(1L);
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails1));
        when(shipmentOrderDao.findByShipmentId(any())).thenReturn(new ArrayList<>());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.attachDetachOrder(shipmentRequest);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void attachDetachOrderTest6_ThrowException() {
        ShipmentOrderAttachDetachRequest shipmentRequest = ShipmentOrderAttachDetachRequest.builder().build();
        shipmentRequest.setOrderDetailsList(List.of(ShipmentOrderAttachDetachRequest.OrderDetails.builder().orderGuid(UUID.fromString("eaf227f3-de85-42b4-8180-cf48ccf568f9")).build()));
        shipmentRequest.setShipmentGuid(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06"));
        shipmentRequest.setEvent("DETACHED");
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().build();
        shipmentDetails1.setId(1L);

        ShipmentOrder shipmentOrder = ShipmentOrder.builder().build();
        shipmentOrder.setId(1L);
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.attachDetachOrder(shipmentRequest);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void attachDetachOrderTest7_ThrowException() {
        ShipmentOrderAttachDetachRequest shipmentRequest = ShipmentOrderAttachDetachRequest.builder().build();
        shipmentRequest.setOrderDetailsList(List.of(ShipmentOrderAttachDetachRequest.OrderDetails.builder().orderGuid(UUID.fromString("eaf227f3-de85-42b4-8180-cf48ccf568f9")).build()));
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.attachDetachOrder(shipmentRequest);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void attachDetachOrderTest9_ThrowException() {
        ShipmentOrderAttachDetachRequest shipmentRequest = ShipmentOrderAttachDetachRequest.builder().build();
        shipmentRequest.setOrderDetailsList(List.of(ShipmentOrderAttachDetachRequest.OrderDetails.builder().orderGuid(UUID.fromString("eaf227f3-de85-42b4-8180-cf48ccf568f9")).build()));
        shipmentRequest.setShipmentGuid(UUID.fromString("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06"));
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.attachDetachOrder(shipmentRequest);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

//    @Test
    void testUpdateThrowsValidatonExceptionIfStaleShipmentIsSavedAgain() throws RunnerException {
        Long shipmentId = 1L;
        Long linkedConsolidationId = 1L;
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder().id(JsonNullable.of(1L)).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .shipmentId("AIR-CAN-00001")
                .shipmentCreatedOn(LocalDateTime.now())
                .containersList(new HashSet<>(Arrays.asList(Containers.builder().build())))
                .jobType(Constants.SHIPMENT_TYPE_DRT)
                .consignee(Parties.builder().orgCode("org1").build())
                .consigner(Parties.builder().orgCode("org2").build())
                .build();
        shipmentDetails.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(shipmentPatchRequest).build();

        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder()
                .shipmentId(shipmentId)
                .consolidationId(linkedConsolidationId)
                .isAttachmentDone(true)
                .build();

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentDetailsMapper).update(any(), any());
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(List.of(consoleShipmentMapping));

        String errorMessage = ShipmentConstants.STALE_SHIPMENT_UPDATE_ERROR;
        Exception e = assertThrows(RunnerException.class, () -> shipmentService.partialUpdate(commonRequestModel, false));
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void fetchBillChargesShipmentList_SuccessTest(){
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().pageNo(1).pageSize(5).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(
                "9f6acf30-3e62-4d3e-991e-a990fe00f069", listCommonRequest);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setJobType(Constants.JOB_TYPE_CLB);
        shipmentDetails.setIncoterms("CFR");

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("Origin");
        carrierDetails.setOriginPort("OriginPort");
        carrierDetails.setDestinationPort("Port of Discharge");
        carrierDetails.setDestination("Destination");
        shipmentDetails.setCarrierDetails(carrierDetails);

        Parties client = Parties.builder().orgCode("1").addressCode("add").build();
        shipmentDetails.setClient(client);
        shipmentDetails.setConsigner(client);
        shipmentDetails.setConsignee(client);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(shipmentDao.findByGuid(UUID.fromString("9f6acf30-3e62-4d3e-991e-a990fe00f069"))).thenReturn(Optional.of(shipmentDetails));

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(shipmentDetails);

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        var expectedResponse = ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(), shipmentDetailsPage.getTotalElements());
        ResponseEntity<IRunnerResponse> result = shipmentService.fetchBillChargesShipmentList(commonRequestModel);

        assertEquals(expectedResponse, result);
    }


    @Test
    void fetchBillChargesShipmentList_SuccessTest2(){
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().pageNo(1).pageSize(5).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(
                "9f6acf30-3e62-4d3e-991e-a990fe00f069", listCommonRequest);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        CarrierDetails carrierDetails = new CarrierDetails();
        shipmentDetails.setCarrierDetails(carrierDetails);
        Parties client = Parties.builder().build();
        shipmentDetails.setClient(client);
        shipmentDetails.setConsigner(client);
        shipmentDetails.setConsignee(client);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(shipmentDao.findByGuid(UUID.fromString("9f6acf30-3e62-4d3e-991e-a990fe00f069"))).thenReturn(Optional.of(shipmentDetails));

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(shipmentDetails);

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        var expectedResponse = ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(), shipmentDetailsPage.getTotalElements());
        ResponseEntity<IRunnerResponse> result = shipmentService.fetchBillChargesShipmentList(commonRequestModel);

        assertEquals(expectedResponse, result);
    }

    @Test
    void fetchBillChargesShipmentList_SuccessTest3(){
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().pageNo(1).pageSize(5).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(
                "9f6acf30-3e62-4d3e-991e-a990fe00f069", listCommonRequest);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        when(shipmentDao.findByGuid(UUID.fromString("9f6acf30-3e62-4d3e-991e-a990fe00f069"))).thenReturn(Optional.of(shipmentDetails));

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(shipmentDetails);

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        var expectedResponse = ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(shipmentDetailsList),
                shipmentDetailsPage.getTotalPages(), shipmentDetailsPage.getTotalElements());
        ResponseEntity<IRunnerResponse> result = shipmentService.fetchBillChargesShipmentList(commonRequestModel);

        assertEquals(expectedResponse, result);
    }

    @Test
    void fetchBillChargesShipmentList_GuidMissingTest(){
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fetchBillChargesShipmentList(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void fetchBillChargesShipmentList_EmptyDataTest(){
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest("9f6acf30-3e62-4d3e-991e-a990fe00f069");
        when(shipmentDao.findByGuid(UUID.fromString("9f6acf30-3e62-4d3e-991e-a990fe00f069"))).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fetchBillChargesShipmentList(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void testAirDGValidations_Error1() {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails1.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        consolidationDetails1.setHazardous(true);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(List.of(ConsoleShipmentMapping.builder().build()));
        assertThrows(RunnerException.class, () -> shipmentService.dgValidations(shipmentDetails1, consolidationDetails1, 1));
    }

    @Test
    void testAirDGValidations_Error() {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setTransportMode(TRANSPORT_MODE_AIR);
        consolidationDetails1.setContainerCategory(Constants.SHIPMENT_TYPE_LCL);
        consolidationDetails1.setHazardous(true);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(List.of(ConsoleShipmentMapping.builder().build(), ConsoleShipmentMapping.builder().build()));
        assertThrows(RunnerException.class, () -> shipmentService.dgValidations(shipmentDetails1, consolidationDetails1, 0));
    }

    @Test
    void testRequestInterBranchConsole_InterBranchImportShipment_Error() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .direction(Constants.DIRECTION_IMP)
                .build();
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(ConsolidationDetails.builder().hazardous(true).transportMode(TRANSPORT_MODE_AIR).build()));
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        var response = spyService.requestInterBranchConsole(1L, 2L, "");
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testRequestInterBranchConsole_InterBranchImportShipment_() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .direction(Constants.DIRECTION_IMP)
                .build();
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(ConsolidationDetails.builder().transportMode(TRANSPORT_MODE_AIR).build()));
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        var response = spyService.requestInterBranchConsole(1L, 2L, "");
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testRequestInterBranchConsole_InterBranchImportShipment_Error1() throws RunnerException {
        ShipmentService spyService = spy(shipmentService);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .direction(Constants.DIRECTION_IMP)
                .containsHazardous(true)
                .build();
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(ConsolidationDetails.builder().transportMode(TRANSPORT_MODE_AIR).build()));
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        var response = spyService.requestInterBranchConsole(1L, 2L, "");
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testChangeContainerWtVolOnDetach_FCL() throws RunnerException {
        shipmentDetails.getPackingList().add(new Packing());
        shipmentDetails.getPackingList().add(new Packing());
        shipmentDetails.getPackingList().get(0).setContainerId(1L);
        shipmentDetails.getPackingList().get(1).setContainerId(1L);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setContainersList(Set.of(Containers.builder().build()));
        shipmentService.changeContainerWtVolOnDetach(objectMapper.convertValue(shipmentDetails, ShipmentRequest.class), new ArrayList<>(shipmentDetails.getContainersList()));
        verify(containerDao, times(1)).saveAll(any());
    }

    @Test
    void testChangeContainerWtVolOnDetach_FCL_NullPacks() throws RunnerException {
        shipmentDetails.setPackingList(null);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails.setContainersList(Set.of(Containers.builder().build()));
        shipmentService.changeContainerWtVolOnDetach(objectMapper.convertValue(shipmentDetails, ShipmentRequest.class), new ArrayList<>(shipmentDetails.getContainersList()));
        verify(containerDao, times(1)).saveAll(any());
    }

    @Test
    void testChangeContainerWtVolOnDetach_LCL() throws RunnerException {
        shipmentDetails.getPackingList().add(new Packing());
        shipmentDetails.getPackingList().add(new Packing());
        shipmentDetails.getPackingList().get(0).setContainerId(1L);
        shipmentDetails.getPackingList().get(1).setContainerId(1L);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipmentDetails.setContainersList(Set.of(Containers.builder().build()));
        shipmentDetails.getContainersList().iterator().next().setId(1L);
        shipmentService.changeContainerWtVolOnDetach(objectMapper.convertValue(shipmentDetails, ShipmentRequest.class), new ArrayList<>(shipmentDetails.getContainersList()));
        verify(containerDao, times(1)).saveAll(any());
    }

    @Test
    void testCancel_ShipmentExists() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setId(1L);
        shipment.setGuid(UUID.randomUUID());
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipment));
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().IsMAWBColoadingEnabled(true).build());

        var shipmentServiceSpy = Mockito.spy(shipmentService);
        doNothing().when(dependentServiceHelper).pushShipmentDataToDependentService(any(), anyBoolean(), anyBoolean(), any());
        mockTenantSettings();

        // Act
        ResponseEntity<IRunnerResponse> response = shipmentServiceSpy.cancel(commonRequestModel);

        // Assert
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(ShipmentStatus.Cancelled.getValue(), shipment.getStatus());
        verify(shipmentDao).update(shipment, false);
        verify(shipmentSync).sync(any(), any(), any(), any(), anyBoolean());
        verify(consoleShipmentMappingDao).deletePendingStateByShipmentId(anyLong());
    }

    @Test
    void testCancel_ShipmentDoesNotExist() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        when(shipmentDao.findById(1L)).thenReturn(Optional.empty());

        // Act & Assert
        RunnerException exception = assertThrows(RunnerException.class, () -> {
            shipmentService.cancel(commonRequestModel);
        });
        assertEquals(DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG, exception.getMessage());
    }

    @Test
    void retrieveByIdWithShipmentStatusTest() {
        var shipId = 1L;
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(shipId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setId(shipId);
        shipmentDetails.setGuid(UUID.randomUUID());
        ShipmentDetailsResponse mockShipmentDetailsResponse = ShipmentDetailsResponse.builder().status(1).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(modelMapper.map(any(), any())).thenReturn(mockShipmentDetailsResponse);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.retrieveById(commonRequestModel);
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(runnerResponse.getData(), ShipmentDetailsResponse.class);
        assertEquals(ShipmentStatus.fromValue(1).toString(), shipmentDetailsResponse.getShipmentStatus());
    }

    @Test
    void retrieveByIdWithInvalidStatusTest() {
        var shipId = 1L;
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(shipId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setId(shipId);
        shipmentDetails.setGuid(UUID.randomUUID());
        ShipmentDetailsResponse mockShipmentDetailsResponse = ShipmentDetailsResponse.builder().status(Integer.MAX_VALUE).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(modelMapper.map(any(), any())).thenReturn(mockShipmentDetailsResponse);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.retrieveById(commonRequestModel);
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        ShipmentDetailsResponse shipmentDetailsResponse = objectMapper.convertValue(runnerResponse.getData(), ShipmentDetailsResponse.class);
        assertNull(shipmentDetailsResponse.getShipmentStatus());
    }

    @Test
    void testRetrieveForNTEAuthError() {
        when(shipmentDao.findShipmentByIdWithQuery(any())).thenReturn(Optional.of(shipmentDetails));
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        ResponseEntity<IRunnerResponse> response = shipmentService.retrieveForNTE(CommonRequestModel.buildRequest(commonGetRequest));
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testRetrieveForNTERetrievalError() {
        when(shipmentDao.findShipmentByIdWithQuery(any())).thenReturn(Optional.empty());
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        ResponseEntity<IRunnerResponse> response = shipmentService.retrieveForNTE(CommonRequestModel.buildRequest(commonGetRequest));
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testRetrieveForNTEIdNullError() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().build();
        ResponseEntity<IRunnerResponse> response = shipmentService.retrieveForNTE(CommonRequestModel.buildRequest(commonGetRequest));
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testRetrieveForNTE_InvalidNTE() {
        shipmentDetails.setReceivingBranch(TenantContext.getCurrentTenant().longValue());
        when(shipmentDao.findShipmentByIdWithQuery(any())).thenReturn(Optional.of(shipmentDetails));
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        shipmentService.retrieveForNTE(CommonRequestModel.buildRequest(commonGetRequest));
    }

    @Test
    void testRetrieveForNTE() {
        shipmentDetails.setStatus(null);
        TriangulationPartner triangulationPartner = TriangulationPartner.builder()
                .triangulationPartner(TenantContext.getCurrentTenant().longValue()).build();
        shipmentDetails.setTriangulationPartnerList(List.of(triangulationPartner));
        when(shipmentDao.findShipmentByIdWithQuery(any())).thenReturn(Optional.of(shipmentDetails));
        when(modelMapper.map(any(), eq(ShipmentDetailsResponse.class))).thenReturn(objectMapper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        ResponseEntity<IRunnerResponse> response = shipmentService.retrieveForNTE(CommonRequestModel.buildRequest(commonGetRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testRetrieveForNTE1() {
        shipmentDetails.setReceivingBranch(TenantContext.getCurrentTenant().longValue());
        List<TriangulationPartner> triangulationPartners = List.of(
                TriangulationPartner.builder().triangulationPartner(1L).build()
        );
        testConsol.setTriangulationPartnerList(triangulationPartners);
        testConsol.setReceivingBranch(1L);
        shipmentDetails.setConsolidationList(Set.of(testConsol));
        when(shipmentDao.findShipmentByIdWithQuery(any())).thenReturn(Optional.of(shipmentDetails));
        TenantContext.setCurrentTenant(1);
        when(modelMapper.map(any(), eq(ShipmentDetailsResponse.class))).thenReturn(objectMapper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        ResponseEntity<IRunnerResponse> response = shipmentService.retrieveForNTE(CommonRequestModel.buildRequest(commonGetRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testRetrieveForNTE2() {
        shipmentDetails.setStatus(100);
        TriangulationPartner triangulationPartner = TriangulationPartner.builder()
                .triangulationPartner(TenantContext.getCurrentTenant().longValue()).build();
        shipmentDetails.setTriangulationPartnerList(List.of(triangulationPartner));
        when(shipmentDao.findShipmentByIdWithQuery(any())).thenReturn(Optional.of(shipmentDetails));
        when(modelMapper.map(any(), eq(ShipmentDetailsResponse.class))).thenReturn(objectMapper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        ResponseEntity<IRunnerResponse> response = shipmentService.retrieveForNTE(CommonRequestModel.buildRequest(commonGetRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testRetrieveForNTE3() {
        shipmentDetails.setReceivingBranch(TenantContext.getCurrentTenant().longValue());
        List<TriangulationPartner> triangulationPartners = List.of(
                TriangulationPartner.builder().triangulationPartner(1L).build()
        );
        testConsol.setTriangulationPartnerList(triangulationPartners);
        shipmentDetails.setConsolidationList(Set.of(testConsol));
        when(shipmentDao.findShipmentByIdWithQuery(any())).thenReturn(Optional.of(shipmentDetails));
        TenantContext.setCurrentTenant(1);
        when(modelMapper.map(any(), eq(ShipmentDetailsResponse.class))).thenReturn(objectMapper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        ResponseEntity<IRunnerResponse> response = shipmentService.retrieveForNTE(CommonRequestModel.buildRequest(commonGetRequest));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void completeUpdateNTETest() throws RunnerException {
        shipmentDetails.setId(1L);
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        ShipmentDetails mockShipment = objectMapper.convertValue(shipmentDetails, ShipmentDetails.class);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        // Mock
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails.setConsolidationList(new HashSet<>())
                .setContainersList(new HashSet<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(jsonHelper.convertValueToList(any(), eq(RoutingsRequest.class))).thenReturn(List.of(new RoutingsRequest()));
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        mockShipmentSettings();
        mockTenantSettings();
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeUpdateNTETest2() throws RunnerException {
        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = objectMapper.convertValue(shipmentDetails, ShipmentDetails.class);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        // Mock
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails.setConsolidationList(new HashSet<>())
                .setContainersList(new HashSet<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(jsonHelper.convertValueToList(any(), eq(RoutingsRequest.class))).thenReturn(List.of(new RoutingsRequest()));
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        mockShipmentSettings();
        mockTenantSettings();
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeUpdateNTETest3() throws RunnerException {
        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = objectMapper.convertValue(shipmentDetails, ShipmentDetails.class);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(12L).build();
        // Mock
        shipmentDetails.setReceivingBranch(1L);
        shipmentDetails.setTriangulationPartnerList(List.of(triangulationPartner));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails.setConsolidationList(new HashSet<>())
                .setContainersList(new HashSet<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(jsonHelper.convertValueToList(any(), eq(RoutingsRequest.class))).thenReturn(List.of(new RoutingsRequest()));
        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        mockShipmentSettings();
        mockTenantSettings();
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void createShipmentAndNTEsuccess2() throws RunnerException {
        UserContext.getUser().setPermissions(new HashMap<>());
        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setShipmentId("AIR-CAN-00001");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).build());


        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        mockShipmentRequest.setEventsList(List.of(EventsRequest.builder()
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .eventCode("eventType").build()));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);

        // Test
        mockShipmentSettings();
        when(commonUtils.getCurrentTenantSettings()).thenReturn(V1TenantSettingsResponse.builder().transportModeConfig(true).build());

        when(commonUtils.isTransportModeValid(anyString(), anyString(), any())).thenReturn(true);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.create(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_EligibleForNetworkTransfer() {
        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(1L).build();
        TriangulationPartner triangulationPartner1 = TriangulationPartner.builder().triangulationPartner(2L).build();
        TriangulationPartner triangulationPartner2 = TriangulationPartner.builder().triangulationPartner(3L).build();
        TriangulationPartner triangulationPartner3 = TriangulationPartner.builder().triangulationPartner(4L).build();
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setReceivingBranch(1L);
        shipmentDetails.setDirection("Inbound");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setTriangulationPartnerList(List.of(triangulationPartner, triangulationPartner1, triangulationPartner2));

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setReceivingBranch(1L);
        oldEntity.setTriangulationPartnerList(List.of(triangulationPartner2, triangulationPartner3));
        when(commonUtils.getTriangulationPartnerList(shipmentDetails.getTriangulationPartnerList())).thenReturn(List.of(1L, 2L, 3L));
        when(commonUtils.getTriangulationPartnerList(oldEntity.getTriangulationPartnerList())).thenReturn(List.of(3L, 4L));
        // Act
        shipmentService.createOrUpdateNetworkTransferEntity(shipmentDetails, oldEntity);

        // Verify new tenant IDs processing
        verify(networkTransferService, times(1)).processNetworkTransferEntity(eq(1L), isNull(), eq(Constants.SHIPMENT), eq(shipmentDetails), isNull(), eq(Constants.DIRECTION_CTS), isNull(), anyBoolean());
        verify(networkTransferService, times(1)).processNetworkTransferEntity(eq(2L), isNull(), eq(Constants.SHIPMENT), eq(shipmentDetails), isNull(), eq(Constants.DIRECTION_CTS), isNull(), anyBoolean());

        // Verify old tenant IDs processing for removal
        verify(networkTransferService, times(1)).processNetworkTransferEntity(isNull(), eq(4L), eq(Constants.SHIPMENT), eq(shipmentDetails), isNull(), eq(Constants.DIRECTION_CTS), isNull(), anyBoolean());
    }

    @Test
    public void testCreateOrUpdateNetworkTransferEntity_NotEligibleForNetworkTransfer() {
        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(3L).build();
        TriangulationPartner triangulationPartner1 = TriangulationPartner.builder().triangulationPartner(4L).build();
        TriangulationPartner triangulationPartner2 = TriangulationPartner.builder().triangulationPartner(5L).build();
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setReceivingBranch(1L);
        shipmentDetails.setDirection("NonEligibleDirection"); // Non-eligible direction
        shipmentDetails.setTransportMode("NonEligibleTransportMode"); // Non-eligible transport mode
        shipmentDetails.setJobType("NonEligibleJobType"); // Non-eligible job type

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setId(100L); // Mocked ID for oldEntity
        oldEntity.setReceivingBranch(2L); // Old receiving branch
        oldEntity.setTriangulationPartnerList(List.of(triangulationPartner, triangulationPartner1, triangulationPartner2)); // Old triangulation partners

        // Act
        shipmentService.createOrUpdateNetworkTransferEntity(shipmentDetails, oldEntity);

        // Assert and Verify

        // Verify that deleteValidNetworkTransferEntity is called for oldEntity's receivingBranch
        verify(networkTransferService, times(1))
                .deleteValidNetworkTransferEntity(eq(2L), eq(100L), eq(Constants.SHIPMENT));

        // Verify that deleteValidNetworkTransferEntity is called for each triangulation partner
        verify(networkTransferService, times(1))
                .deleteValidNetworkTransferEntity(eq(3L), eq(100L), eq(Constants.SHIPMENT));
        verify(networkTransferService, times(1))
                .deleteValidNetworkTransferEntity(eq(4L), eq(100L), eq(Constants.SHIPMENT));
        verify(networkTransferService, times(1))
                .deleteValidNetworkTransferEntity(eq(5L), eq(100L), eq(Constants.SHIPMENT));

        // Ensure no processNetworkTransferEntity is invoked
        verify(networkTransferService, never()).processNetworkTransferEntity(any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void createShipmentAndAutoMaticTransferSuccess() throws RunnerException {
        UserContext.getUser().setPermissions(new HashMap<>());
        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setShipmentId("AIR-CAN-00001");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(Constants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setReceivingBranch(1L);
        shipmentDetails.getCarrierDetails().setEta(LocalDateTime.now());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isAutomaticTransferEnabled(true).isNetworkTransferEntityEnabled(true).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);

        // Test
        mockShipmentSettings();
        mockTenantSettings();

        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurationsList = Collections.singletonList(V1TenantSettingsResponse.FileTransferConfigurations.builder().build());
        when(quartzJobInfoService.getActiveFileTransferConfigurations(any())).thenReturn(fileTransferConfigurationsList);
        when(quartzJobInfoService.getQuartzJobTime(any(),any(),any(),any(), any())).thenReturn(LocalDateTime.now());
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.create(commonRequestModel);
        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void completeUpdateAutoMaticTransferTest2() throws RunnerException {
        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = objectMapper.convertValue(shipmentDetails, ShipmentDetails.class);
        mockShipment.setShipmentId("AIR-CAN-00001");
        mockShipment.setTransportMode(TRANSPORT_MODE_AIR);
        mockShipment.setJobType(Constants.SHIPMENT_TYPE_DRT);
        mockShipment.setDirection(Constants.DIRECTION_EXP);
        mockShipment.setReceivingBranch(1L);
        mockShipment.setTriangulationPartner(12L);
        mockShipment.getCarrierDetails().setEta(LocalDateTime.now());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);


        // Mock
        ShipmentDetails mockShipment2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now()).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();
        mockShipment2.setId(shipmentDetails.getId());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(mockShipment2));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);

        ShipmentDetails mockShipment3 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();
        when(jsonHelper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment3);
        when(jsonHelper.convertValueToList(any(), eq(RoutingsRequest.class))).thenReturn(List.of(new RoutingsRequest()));

        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurationsList = Collections.singletonList(V1TenantSettingsResponse.FileTransferConfigurations.builder().build());
        when(quartzJobInfoService.getActiveFileTransferConfigurations(any())).thenReturn(fileTransferConfigurationsList);
        when(quartzJobInfoService.getQuartzJobTime(any(),any(),any(),any(), any())).thenReturn(LocalDateTime.now());
        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.save(any(QuartzJobInfo.class))).thenReturn(quartzJobInfo);
        when(quartzJobInfoDao.findByJobFilters(any(), anyLong(), anyString())).thenReturn(Optional.of(quartzJobInfo));
        mockShipmentSettings();
        mockTenantSettings();

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdate(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void triggerAutomaticTransferTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurationsList = Collections.singletonList(V1TenantSettingsResponse.FileTransferConfigurations.builder().build());
        when(quartzJobInfoService.getActiveFileTransferConfigurations(any())).thenReturn(fileTransferConfigurationsList);
        when(quartzJobInfoService.getQuartzJobTime(any(),any(),any(),any(), any())).thenReturn(LocalDateTime.now());
        shipmentService.triggerAutomaticTransfer(shipmentDetails2, null, false);

        verify(quartzJobInfoService, times(1)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest13() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(null).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        ShipmentDetails oldEntity = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        shipmentService.triggerAutomaticTransfer(shipmentDetails2, oldEntity, false);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest2() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentService.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, false);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest3() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.TRANSFERRED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentService.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, false);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest4() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentService.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, false);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest5() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.QUEUED).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentService.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, false);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest6() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        when(quartzJobInfoService.getQuartzJobTime(any(), any(), any(), any(), any())).thenReturn(LocalDateTime.now());
        when(quartzJobInfoDao.save(any())).thenReturn(quartzJobInfo);
        when(quartzJobInfoService.isJobWithNamePresent(any())).thenReturn(true);
        when(quartzJobInfoService.updateSimpleJob(any())).thenReturn(null);
        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurationsList = Collections.singletonList(V1TenantSettingsResponse.FileTransferConfigurations.builder().build());
        when(quartzJobInfoService.getActiveFileTransferConfigurations(any())).thenReturn(fileTransferConfigurationsList);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentService.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, true);

        verify(quartzJobInfoService, times(1)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest7() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentService.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, true);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest8() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.QUEUED).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentService.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, true);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest9() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.QUEUED).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.empty());
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentService.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, true);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest10() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurationsList = Collections.singletonList(V1TenantSettingsResponse.FileTransferConfigurations.builder().build());
        when(quartzJobInfoService.getActiveFileTransferConfigurations(any())).thenReturn(fileTransferConfigurationsList);
        when(quartzJobInfoService.getQuartzJobTime(any(), any(), any(), any(), any())).thenReturn(LocalDateTime.now());
        when(quartzJobInfoDao.save(any())).thenReturn(quartzJobInfo);
        when(quartzJobInfoService.isJobWithNamePresent(any())).thenReturn(false);
        when(quartzJobInfoService.createSimpleJob(any())).thenReturn(null);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentService.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, true);

        verify(quartzJobInfoService, times(1)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest11() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        var dateTime = LocalDateTime.now().plusHours(4);
        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(dateTime).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        ShipmentDetails shipmentDetails3 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).
                carrierDetails(CarrierDetails.builder().eta(dateTime).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        when(quartzJobInfoService.getQuartzJobTime(any(), any(), any(), any(), any())).thenReturn(LocalDateTime.now());
        when(quartzJobInfoDao.save(any())).thenReturn(quartzJobInfo);
        when(quartzJobInfoService.isJobWithNamePresent(any())).thenReturn(false);
        when(quartzJobInfoService.createSimpleJob(any())).thenReturn(null);
        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurationsList = Collections.singletonList(V1TenantSettingsResponse.FileTransferConfigurations.builder().build());
        when(quartzJobInfoService.getActiveFileTransferConfigurations(any())).thenReturn(fileTransferConfigurationsList);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentService.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails3, false);

        verify(quartzJobInfoService, times(1)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest12() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        var dateTime = LocalDateTime.now().plusHours(4);
        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(dateTime).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        ShipmentDetails shipmentDetails3 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(2L).
                carrierDetails(CarrierDetails.builder().eta(dateTime).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        when(quartzJobInfoService.getQuartzJobTime(any(), any(), any(), any(), any())).thenReturn(LocalDateTime.now());
        when(quartzJobInfoDao.save(any())).thenReturn(quartzJobInfo);
        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurationsList = Collections.singletonList(V1TenantSettingsResponse.FileTransferConfigurations.builder().build());
        when(quartzJobInfoService.getActiveFileTransferConfigurations(any())).thenReturn(fileTransferConfigurationsList);
        when(quartzJobInfoService.isJobWithNamePresent(any())).thenReturn(false);
        when(quartzJobInfoService.createSimpleJob(any())).thenReturn(null);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        shipmentService.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails3, false);

        verify(quartzJobInfoService, times(1)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void testPopulateOriginDestinationAgentDetailsForBookingShipment_NoConsolidationList() {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setConsolidationList(Collections.emptySet());

        shipmentService.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetails1);

        verifyNoInteractions(commonUtils, v1ServiceUtil, consolidationDetailsDao);
    }

    @Test
    void testPopulateOriginDestinationAgentDetailsForBookingShipment_ConsolidationDetailsNotInterBranch() {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        Parties parties = Parties.builder().orgId("agent").build();
        consolidationDetails1.setSendingAgent(parties);
        consolidationDetails1.setReceivingAgent(parties);
        consolidationDetails1.setInterBranchConsole(false);
        shipmentDetails1.setConsolidationList(Set.of(consolidationDetails1));

        shipmentService.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetails1);

        verifyNoInteractions(v1ServiceUtil, consolidationDetailsDao);
    }

    @Test
    void testPopulateOriginDestinationAgentDetailsForBookingShipment_ConsolidationDetailsNotInterBranch2() {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        Parties parties = Parties.builder().orgId("agent").build();
        consolidationDetails1.setReceivingAgent(parties);
        consolidationDetails1.setInterBranchConsole(false);
        shipmentDetails1.setConsolidationList(Set.of(consolidationDetails1));

        shipmentService.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetails1);

        verifyNoInteractions(v1ServiceUtil, consolidationDetailsDao);
    }

    @Test
    void testPopulateOriginDestinationAgentDetailsForBookingShipment_ConsolidationDetailsNotInterBranch3() {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setAdditionalDetails(new AdditionalDetails());
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        Parties parties = Parties.builder().orgId("agent").build();
        consolidationDetails1.setSendingAgent(parties);
        consolidationDetails1.setReceivingAgent(parties);
        consolidationDetails1.setInterBranchConsole(false);
        shipmentDetails1.setConsolidationList(Set.of(consolidationDetails1));

        shipmentService.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetails1);

        verifyNoInteractions(v1ServiceUtil, consolidationDetailsDao);
    }

    @Test
    void testPopulateOriginDestinationAgentDetailsForBookingShipment_ConsolidationDetailsInterBranch() {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setInterBranchConsole(true);
        shipmentDetails1.setConsolidationList(Set.of(consolidationDetails1));

        shipmentService.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetails1);

        verifyNoInteractions(commonUtils, v1ServiceUtil, consolidationDetailsDao);
    }

    @Test
    void testPopulateOriginDestinationAgentDetailsForBookingShipment_SetDefaultExportBroker() {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setDirection(Constants.DIRECTION_EXP);

        when(v1ServiceUtil.getDefaultAgentOrgParty(null)).thenReturn(new Parties());

        shipmentService.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetails1);

        verify(v1ServiceUtil).getDefaultAgentOrgParty(null);
        assertNotNull(shipmentDetails1.getAdditionalDetails().getExportBroker());
    }

    @Test
    void testPopulateOriginDestinationAgentDetailsForBookingShipment_SetDefaultImportBroker() {
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setDirection(Constants.DIRECTION_IMP);

        when(v1ServiceUtil.getDefaultAgentOrgParty(null)).thenReturn(new Parties());

        shipmentService.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetails1);

        verify(v1ServiceUtil).getDefaultAgentOrgParty(null);
        assertNotNull(shipmentDetails1.getAdditionalDetails().getImportBroker());
    }

    @Test
    void testPopulateOriginDestinationAgentDetailsForBookingShipment_SetConsolidationAgent() {
        ConsolidationDetails consolidationDetails1 = mock(ConsolidationDetails.class);
        Parties agent = new Parties();
        agent.setOrgId("SendingAgent");

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setConsolidationList(Set.of(consolidationDetails1));

        AdditionalDetails additionalDetails = new AdditionalDetails();
        additionalDetails.setExportBroker(Parties.builder().orgId("ExportBroker").build());
        additionalDetails.setImportBroker(Parties.builder().orgId("ImportBroker").build());
        shipmentDetails1.setAdditionalDetails(additionalDetails);

        shipmentService.populateOriginDestinationAgentDetailsForBookingShipment(shipmentDetails1);

        verify(consolidationDetailsDao).save(any(), anyBoolean());
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_EligibleForNetworkTransfer_InterConsole() {
        // Arrange
        ShipmentDetails newShipmentDetails = new ShipmentDetails();
        newShipmentDetails.setReceivingBranch(1L);
        newShipmentDetails.setDirection("Inbound");
        newShipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        newShipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        newShipmentDetails.setDirection(DIRECTION_EXP);
        ConsolidationDetails consolidationDetails1 = testConsol;
        consolidationDetails1.setInterBranchConsole(true);
        consolidationDetails1.setShipmentsList(new HashSet<>(Collections.singletonList(newShipmentDetails)));
        newShipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setReceivingBranch(1L);

        // Act
        shipmentService.createOrUpdateNetworkTransferEntity(newShipmentDetails, oldEntity);

        // Verify old tenant IDs processing for removal
        verify(networkTransferService, times(1)).processNetworkTransferEntity(eq(1L), isNull(), eq(Constants.SHIPMENT), eq(newShipmentDetails), isNull(), eq(IMP), isNull(), eq(true));
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_EligibleForNetworkTransfer_OldInterConsole() {
        // Arrange
        ShipmentDetails newShipmentDetails = new ShipmentDetails();
        newShipmentDetails.setReceivingBranch(1L);
        newShipmentDetails.setDirection("Inbound");
        newShipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        newShipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        newShipmentDetails.setDirection(DIRECTION_EXP);
        ConsolidationDetails consolidationDetails1 = testConsol;
        consolidationDetails1.setInterBranchConsole(true);

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setReceivingBranch(1L);
        oldEntity.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        // Act
        shipmentService.createOrUpdateNetworkTransferEntity(newShipmentDetails, oldEntity);

        // Verify old tenant IDs processing for removal
        verify(networkTransferService, times(1)).deleteValidNetworkTransferEntity(any(), any(), any());
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_EligibleForNetworkTransfer_InterConsole2() {
        // Arrange
        ShipmentDetails newShipmentDetails = new ShipmentDetails();
        newShipmentDetails.setReceivingBranch(2L);
        newShipmentDetails.setDirection("Inbound");
        newShipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        newShipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        newShipmentDetails.setDirection(DIRECTION_EXP);
        ConsolidationDetails consolidationDetails1 = testConsol;
        consolidationDetails1.setInterBranchConsole(true);
        consolidationDetails1.setShipmentsList(new HashSet<>(Collections.singletonList(newShipmentDetails)));
        newShipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setReceivingBranch(1L);
        oldEntity.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        // Act
        shipmentService.createOrUpdateNetworkTransferEntity(newShipmentDetails, oldEntity);

        // Verify old tenant IDs processing for removal
        verify(networkTransferService, times(1)).processNetworkTransferEntity(eq(2L), eq(1L), eq(Constants.SHIPMENT), eq(newShipmentDetails), isNull(), eq(IMP), isNull(), eq(true));
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_EligibleForNetworkTransfer_OldInterConsole2() {
        // Arrange
        ShipmentDetails newShipmentDetails = new ShipmentDetails();
        newShipmentDetails.setId(1L);
        newShipmentDetails.setDirection("Inbound");
        newShipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        newShipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        newShipmentDetails.setDirection(DIRECTION_EXP);
        ConsolidationDetails consolidationDetails1 = testConsol;
        consolidationDetails1.setInterBranchConsole(true);
        consolidationDetails1.setShipmentsList(new HashSet<>(Collections.singletonList(newShipmentDetails)));
        newShipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(1L);
        oldEntity.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(Collections.singletonList(NetworkTransfer.builder().entityId(1L).isInterBranchEntity(true).build()));

        // Act
        shipmentService.createOrUpdateNetworkTransferEntity(newShipmentDetails, oldEntity);

        // Verify old tenant IDs processing for removal
        verify(networkTransferDao, times(1)).deleteAndLog(any(), any());
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_EligibleForNetworkTransfer_InterConsole3() {
        // Arrange
        ShipmentDetails newShipmentDetails = new ShipmentDetails();
        newShipmentDetails.setId(1L);
        newShipmentDetails.setReceivingBranch(2L);
        newShipmentDetails.setDirection("Inbound");
        newShipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        newShipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        newShipmentDetails.setDirection(DIRECTION_EXP);
        ConsolidationDetails consolidationDetails1 = testConsol;
        consolidationDetails1.setInterBranchConsole(true);
        consolidationDetails1.setReceivingBranch(2L);
        consolidationDetails1.setShipmentsList(new HashSet<>(Collections.singletonList(newShipmentDetails)));
        newShipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(1L);
        oldEntity.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        Map<String, Object> entityPayload = Map.of("abcd", 1);
        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(Collections.singletonList(NetworkTransfer.builder().entityId(1L).entityPayload(entityPayload).isInterBranchEntity(true).build()));

        // Act
        shipmentService.createOrUpdateNetworkTransferEntity(newShipmentDetails, oldEntity);

        // Verify old tenant IDs processing for removal
        verify(networkTransferDao, times(1)).deleteByIdsAndLog(any());
    }


    @Test
    void testCreateV3() throws RunnerException {
        UserContext.getUser().setPermissions(new HashMap<>());
        ShipmentDetails mockShipment = shipmentDetails;
        mockShipment.setShipmentId("AIR-CAN-00001");
        AdditionalDetails additionalDetails = getmockAdditionalDetails(LocalDateTime.now(), true, true, true);
        mockShipment.setAdditionalDetails(additionalDetails);
        mockShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        LocalDateTime mockDateTime = LocalDateTime.now();

        TrackingServiceApiResponse trackingResponse = new TrackingServiceApiResponse();
        trackingResponse.setContainers(List.of(Container.builder()
                .containerNumber("containerNumber")
                .journey(new Journey())
                .places(List.of())
                .events(List.of(Event.builder()
                        .eventType("eventType")
                        .actualEventTime(DateAndSources.builder()
                                .dateTime(mockDateTime).build()).build())).build()));
        TrackingServiceApiResponse.DateAndSources dateAndSources = new TrackingServiceApiResponse.DateAndSources();
        dateAndSources.setDateTime(mockDateTime);

        trackingResponse.getContainers().get(0).getJourney().setPortOfArrivalAta(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfDepartureAtd(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfArrivalEta(dateAndSources);
        trackingResponse.getContainers().get(0).getJourney().setPortOfDepartureEtd(dateAndSources);

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        mockShipmentRequest.setEventsList(List.of(EventsRequest.builder()
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .eventCode("eventType").build()));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        // Mock
        when(jsonHelper.convertCreateValue(any(), eq(ShipmentDetails.class))).thenReturn(mockShipment);
        mockShipment.setId(1L).setGuid(UUID.randomUUID());
        when(shipmentDao.save(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(any(), eq(ShipmentDetailsResponse.class))).thenReturn(mockShipmentResponse);

        Events eventType = Events.builder()
                .eventCode("eventType")
                .source(Constants.MASTER_DATA_SOURCE_CARGOES_TRACKING)
                .build();
        List<Events> eventTypeList = List.of(eventType);
        when(commonUtils.convertToEntityList(any(), eq(Events.class), any())).thenReturn(eventTypeList);

        // Test
        mockShipmentSettings();
        when(commonUtils.getCurrentTenantSettings()).thenReturn(V1TenantSettingsResponse.builder().transportModeConfig(true).build());

        when(commonUtils.isTransportModeValid(anyString(), anyString(), any())).thenReturn(true);
        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.createV3(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void testCompleteUpdateV3() throws RunnerException {
        shipmentDetails.setId(1L);
        ShipmentDetails mockShipment = shipmentDetails;
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).build());

        ShipmentRequest mockShipmentRequest = objectMapper.convertValue(mockShipment, ShipmentRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mockShipmentRequest);
        ShipmentDetailsResponse mockShipmentResponse = objectMapper.convertValue(mockShipment, ShipmentDetailsResponse.class);

        // Mock
        when(shipmentDao.findById(any()))
                .thenReturn(
                        Optional.of(
                                shipmentDetails
                                        .setConsolidationList(new HashSet<>())
                                        .setContainersList(new HashSet<>())));
        when(mockObjectMapper.convertValue(any(), eq(ShipmentDetails.class))).thenReturn(shipmentDetails);

        when(shipmentDao.update(any(), eq(false))).thenReturn(mockShipment);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDetailsMapper.map((ShipmentDetails) any())).thenReturn(mockShipmentResponse);
        mockShipmentSettings();
        when(commonUtils.getCurrentTenantSettings()).thenReturn(V1TenantSettingsResponse.builder().transportModeConfig(true).build());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.completeUpdateV3(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockShipmentResponse), httpResponse);
    }

    @Test
    void testRetrieveByIdV3() {
        var shipId = 1L;
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(shipId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setId(shipId);
        shipmentDetails.setGuid(UUID.randomUUID());

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(notesDao.findByEntityIdAndEntityType(anyLong(), eq(Constants.CUSTOMER_BOOKING))).thenReturn(Arrays.asList(Notes.builder().entityId(1L).build()));
        when(notificationDao.pendingNotificationCountBasedOnEntityIdsAndEntityType(anyList(), anyString())).thenReturn(Map.of(1L, 5));

        when(jsonHelper.convertValueToList(anyList(), eq(NotesResponse.class))).thenReturn(Arrays.asList(NotesResponse.builder().build()));
        when(modelMapper.map(any(), any())).thenReturn(ShipmentDetailsResponse.builder().build());

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.retrieveByIdV3(commonRequestModel, true);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testRetrieveByIdV3ThrowsException() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).build());
        ResponseEntity<IRunnerResponse> responseEntity = shipmentService.retrieveByIdV3(CommonRequestModel.builder().build(), false);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testFullShipmentListV3() throws IOException, IllegalAccessException {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(ShipmentDetails.builder().build());

        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);
        when(modelMapper.map(any(), eq(ShipmentDetailsResponse.class))).thenReturn(new ShipmentDetailsResponse());

        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToFullShipmentList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.fullShipmentsListV3(commonRequestModel);
        assertEquals(expectedResponse, httpResponse);
    }
    @Test
    void testListV3() {
        Criteria criteria = Criteria.builder().fieldName("wayBillNumber").value(1).build();
        ListCommonRequest listCommonRequest = ListCommonRequest.builder().filterCriteria(Arrays.asList(FilterCriteria.builder().criteria(criteria).innerFilter(Arrays.asList(FilterCriteria.builder().build())).build())).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        GuidsListResponse guidsListResponse = new GuidsListResponse();
        when(v1Service.fetchWayBillNumberFilterGuids(any())).thenReturn(guidsListResponse);

        List<ShipmentDetails> shipmentDetailsList = new ArrayList<>();
        shipmentDetailsList.add(new ShipmentDetails());
        PageImpl<ShipmentDetails> shipmentDetailsPage = new PageImpl<>(shipmentDetailsList);
        when(shipmentDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentDetailsPage);

        var expectedResponse = ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements()
        );

        ResponseEntity<IRunnerResponse> httpResponse = shipmentService.listV3(commonRequestModel, false);
        assertEquals(expectedResponse, httpResponse);
    }

}
