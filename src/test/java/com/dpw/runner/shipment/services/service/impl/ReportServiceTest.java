package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.DocumentService.DocumentService;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.DocPages;
import com.dpw.runner.shipment.services.ReportingService.Models.DocUploadRequest;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.AWBLabelReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.ArrivalNoticeReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.BookingConfirmationReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.BookingOrderReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.CSDReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.CargoManifestAirConsolidationReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.CargoManifestAirShipmentReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.DeliveryOrderReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.FCRDocumentReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.HawbReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.HblReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.MawbReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.PickupOrderReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.PreAlertReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.SeawayBillReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.ShipmentCANReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.ShipmentTagsForExteranlServices;
import com.dpw.runner.shipment.services.ReportingService.Reports.TransportInstructionReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Reports.TransportOrderReport;
import com.dpw.runner.shipment.services.ReportingService.ReportsFactory;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.AwbDao;
import com.dpw.runner.shipment.services.dao.impl.ConsolidationDao;
import com.dpw.runner.shipment.services.dao.impl.EventDao;
import com.dpw.runner.shipment.services.dao.impl.HblDao;
import com.dpw.runner.shipment.services.dao.impl.HblReleaseTypeMappingDao;
import com.dpw.runner.shipment.services.dao.impl.HblTermsConditionTemplateDao;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IDocDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.impl.DocumentManagerServiceImpl;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.ReportRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Allocations;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.DocDetails;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.HblReleaseTypeMapping;
import com.dpw.runner.shipment.services.entity.HblTermsConditionTemplate;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entity.enums.PrintType;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service.interfaces.IPickupDeliveryDetailsService;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.itextpdf.text.DocumentException;
import java.util.concurrent.CompletableFuture;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ReportServiceTest extends CommonMocks {

    @InjectMocks
    private ReportService reportService;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private IDocDetailsDao docDetailsDao;

    @Mock
    private IV1Service iv1Service;

    @Mock
    private ReportsFactory reportsFactory;

    @Mock
    private SeawayBillReport seawayBillReport;

    @Mock
    private BookingOrderReport bookingOrderReport;

    @Mock
    private AWBLabelReport awbLabelReport;

    @Mock
    private FCRDocumentReport fcrDocumentReport;

    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Mock
    private IDpsEventService dpsEventService;

    @Mock
    private MawbReport mawbReport;

    @Mock
    private ArrivalNoticeReport arrivalNoticeReport;

    @Mock
    private BookingConfirmationReport bookingConfirmationReport;

    @Mock
    private PickupOrderReport pickupOrderReport;

    @Mock
    private DeliveryOrderReport deliveryOrderReport;

    @Mock
    private PreAlertReport preAlertReport;

    @Mock
    private ShipmentCANReport shipmentCANReport;

    @Mock
    private CargoManifestAirConsolidationReport cargoManifestAirConsolidationReport;

    @Mock
    private CargoManifestAirShipmentReport cargoManifestAirShipmentReport;

    @Mock
    private DocumentService documentService;

    @Mock
    private IEventService eventService;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private ShipmentDao shipmentDao;

    @Mock
    private EventDao eventDao;

    @Mock
    private AwbDao awbDao;

    @Mock
    private ShipmentService shipmentService;

    @Mock
    private HblDao hblDao;

    @Mock
    private MasterDataUtils masterDataUtils;

    private final ExecutorService executorService = Executors.newFixedThreadPool(10);

    @Mock
    private DocumentManagerServiceImpl documentManagerService;

    @Mock
    private ConsolidationDao consolidationDao;

    @Mock
    private DocPages docPages;

    @Mock
    private StringUtility stringUtility;

    @Mock
    private HblReleaseTypeMappingDao hblReleaseTypeMappingDao;

    @Mock
    private HblTermsConditionTemplateDao hblTermsConditionTemplateDao;

    @Mock
    private ShipmentTagsForExteranlServices shipmentTagsForExteranlServices;

    @Mock
    private TransportOrderReport transportOrderReport;

    @Mock
    private HblReport hblReport;

    @Mock
    private HawbReport hawbreport;

    @Mock
    private CSDReport csdReport;

    @Mock
    private ConsolidationService consolidationService;

    @Mock
    private DependentServiceHelper dependentServiceHelper;

    @Mock
    private ReportService self;

    @Mock
    private IPickupDeliveryDetailsService pickupDeliveryDetailsService;

    @Mock
    private ITransportInstructionLegsService transportInstructionLegsService;
    @Mock
    private TransportInstructionReportHelper transportInstructionReport;

    private final String path = "src/test/java/com/dpw/runner/shipment/services/files/";

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
    }

    private static ReportRequest reportRequest;

    @BeforeEach
    void setup() {
        reportRequest = jsonTestUtility.getTestReportRequest();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        reportService.executorService = executorService;
        reportService.executorServiceReport = executorService;
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    @AfterEach
    void tearDown() {
        reportService.executorService.shutdown();
        reportService.executorServiceReport.shutdown();
    }


    @Test
    void getSeawayBillDocumentData()
        throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setSeawayMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setSeawayMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(seawayBillReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(shipmentDao.findById(any())).thenReturn(Optional.of(new ShipmentDetails()));
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void invalidTemplate() {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setSeawayMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setSeawayMainPage("123456789");
        shipmentSettingsDetails2.setSeawayMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(seawayBillReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(null));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Mockito.when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().volumeDecimalPlace(2).build());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);

        Exception e = assertThrows(ValidationException.class, () -> reportService.getDocumentData(commonRequestModel));

        String errorMessage = "Please Upload Valid Template";
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void invalidTemplateId() {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setSeawayMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setSeawayMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(seawayBillReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenThrow(new ValidationException("Invalid Template Id"));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Mockito.when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().volumeDecimalPlace(2).build());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);

        Exception e = assertThrows(ValidationException.class, () ->
                reportService.getDocumentData(commonRequestModel));

        String errorMessage = "Please Upload Valid Template";
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void templateNotExists() {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(seawayBillReport);
        Mockito.when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().volumeDecimalPlace(2).build());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);

        Exception e = assertThrows(ValidationException.class, () ->
                reportService.getDocumentData(commonRequestModel)
        );

        String errorMessage = "Please upload template in branch settings for: SeawayBill";
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void getShipTruckWayDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setShipTruckWayBillMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setShipTruckWayBillMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.SHIP_TRUCKWAY_BILL);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(seawayBillReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getConTruckWayDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setConsTruckWayBillMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setConsTruckWayBillMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CONS_TRUCKWAY_BILL);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(seawayBillReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipTruckDriverDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setShipTruckDriverProof("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setShipTruckDriverProof("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.SHIP_TRUCK_DRIVER_PROOF);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(seawayBillReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getConsTruckDriverDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setConsTruckDriverProof("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setConsTruckDriverProof("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CONS_TRUCK_DRIVER_PROOF);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(seawayBillReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }


    @Test
    void getMAwbDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setMawb("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setMawb("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.MAWB);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.DRAFT);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getMAwbDocumentDataForEAW()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setMawb("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setMawb("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.MAWB);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.DRAFT);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.SPECIAL_HANDLING_CODE, "EAW");
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getMAwbWithOtherAmountDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setMawb("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setMawb("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.MAWB);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(true);
        reportRequest.setPrintType(ReportConstants.DRAFT);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getDMAwbDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setMawb("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setMawb("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.MAWB);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.DRAFT);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        reportRequest.setFromShipment(true);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        when(mawbReport.getData(any())).thenReturn(dataRetrived);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getHawbDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setHawb("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        Awb mockAwb = new Awb();

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setHawb("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.HAWB);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        Mockito.doNothing().when(shipmentService).updateDateAndStatus(any(), any(), any());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getHawbWithOtherAmountDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setHawb("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setHawb("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.HAWB);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(true);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        Awb mockAwb = new Awb();
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        Mockito.doNothing().when(shipmentService).updateDateAndStatus(any(), any(), any());
        Runnable mockRunnable = mock(Runnable.class);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getHawbDraftDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setHawb("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setHawb("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.HAWB);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.DRAFT);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("1,2,3");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getHawbNeutralDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAwbNeutral("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAwbNeutral("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.HAWB);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.NEUTRAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("1,2,3");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        Mockito.doNothing().when(shipmentService).updateDateAndStatus(any(), any(), any());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getHouseBillDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setHouseMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);
        shipmentSettingsDetails.setRestrictBlRelease(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setHouseMainPage("123456789");
        shipmentSettingsDetails2.setHblFooter("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        shipmentSettingsDetails2.setPrintAfterEachPage(true);
        shipmentSettingsDetails2.setRestrictBlRelease(true);
        reportRequest.setReportInfo(ReportConstants.HOUSE_BILL);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        reportRequest.setNoOfCopies("2");
        reportRequest.setFrontTemplateCode("123");
        reportRequest.setBackTemplateCode("123");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.ORIGINALS, 1);
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        dataRetrived.put(ReportConstants.COPY_BILLS, 1);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setAdditionalDetails(new AdditionalDetails());
        shipmentDetails.getAdditionalDetails().setOriginal(1);
        shipmentDetails.getAdditionalDetails().setReleaseType("ORG");
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentDao.update(shipmentDetails, false)).thenReturn(shipmentDetails);
        Hbl hbl = new Hbl();
        hbl.setHblData(new HblDataDto());
        hbl.getHblData().setOriginalSeq(1);
        hbl.getHblData().setVersion(1);
        when(hblDao.findByShipmentId(Long.parseLong(reportRequest.getReportId()))).thenReturn(Arrays.asList(hbl));
        DocumentManagerResponse documentManagerResponse = new DocumentManagerResponse();
        documentManagerResponse.setSuccess(true);
        when(documentManagerService.temporaryFileUpload(any(), any())).thenReturn(documentManagerResponse);
        HblTermsConditionTemplate hblTermsConditionTemplate = new HblTermsConditionTemplate();
        hblTermsConditionTemplate.setTemplateFileName("122333");
        hblTermsConditionTemplate.setIsWaterMarkRequired(true);
        when(hblTermsConditionTemplateDao.getTemplateCode(any(), any(), any())).thenReturn(hblTermsConditionTemplate);


        Runnable mockRunnable = mock(Runnable.class);

        // Define the behavior of the mock
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getHouseBillWithReleaseTypeDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setHouseMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);
        shipmentSettingsDetails.setRestrictBlRelease(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setHouseMainPage("123456789");
        shipmentSettingsDetails2.setHblFooter("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        shipmentSettingsDetails2.setPrintAfterEachPage(true);
        shipmentSettingsDetails2.setRestrictBlRelease(true);
        reportRequest.setReportInfo(ReportConstants.HOUSE_BILL);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        reportRequest.setNoOfCopies("2");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.ORIGINALS, 1);
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        dataRetrived.put(ReportConstants.COPY_BILLS, 1);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setAdditionalDetails(new AdditionalDetails());
        shipmentDetails.getAdditionalDetails().setOriginal(1);
        shipmentDetails.getAdditionalDetails().setReleaseType("ORG");
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentDao.update(shipmentDetails, false)).thenReturn(shipmentDetails);
        Hbl hbl = new Hbl();
        hbl.setHblData(new HblDataDto());
        hbl.getHblData().setOriginalSeq(1);
        hbl.getHblData().setVersion(1);
        when(hblDao.findByShipmentId(Long.parseLong(reportRequest.getReportId()))).thenReturn(Arrays.asList(hbl));
        DocumentManagerResponse documentManagerResponse = new DocumentManagerResponse();
        documentManagerResponse.setSuccess(true);
        documentManagerResponse.setData(new DocumentManagerDataResponse());
        when(documentManagerService.temporaryFileUpload(any(), any())).thenReturn(documentManagerResponse);
        HblReleaseTypeMapping hblReleaseTypeMapping = new HblReleaseTypeMapping();
        hblReleaseTypeMapping.setCopiesPrinted(1);
        when(hblReleaseTypeMappingDao.findByReleaseTypeAndHblId(any(), any())).thenReturn(Arrays.asList(hblReleaseTypeMapping));


        Runnable mockRunnable = mock(Runnable.class);

        // Define the behavior of the mock
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });

        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getHouseBillWithFailedUploadDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setHouseMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);
        shipmentSettingsDetails.setRestrictBlRelease(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setHouseMainPage("123456789");
        shipmentSettingsDetails2.setHblFooter("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        shipmentSettingsDetails2.setPrintAfterEachPage(true);
        shipmentSettingsDetails2.setRestrictBlRelease(true);
        reportRequest.setReportInfo(ReportConstants.HOUSE_BILL);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        reportRequest.setNoOfCopies("2");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.ORIGINALS, 1);
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        dataRetrived.put(ReportConstants.COPY_BILLS, 1);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setAdditionalDetails(new AdditionalDetails());
        shipmentDetails.getAdditionalDetails().setOriginal(1);
        shipmentDetails.getAdditionalDetails().setReleaseType("ORG");
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentDao.update(shipmentDetails, false)).thenReturn(shipmentDetails);
        Hbl hbl = new Hbl();
        hbl.setHblData(new HblDataDto());
        hbl.getHblData().setOriginalSeq(1);
        hbl.getHblData().setVersion(1);
        when(hblDao.findByShipmentId(Long.parseLong(reportRequest.getReportId()))).thenReturn(Arrays.asList(hbl));
        DocumentManagerResponse documentManagerResponse = new DocumentManagerResponse();
        documentManagerResponse.setSuccess(true);
        when(documentManagerService.temporaryFileUpload(any(), any())).thenReturn(documentManagerResponse);
        HblReleaseTypeMapping hblReleaseTypeMapping = new HblReleaseTypeMapping();
        hblReleaseTypeMapping.setCopiesPrinted(1);
        when(hblReleaseTypeMappingDao.findByReleaseTypeAndHblId(any(), any())).thenReturn(Arrays.asList(hblReleaseTypeMapping));


        Runnable mockRunnable = mock(Runnable.class);

        // Define the behavior of the mock
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getHouseBillDraftDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setHouseMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setHouseMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        shipmentSettingsDetails2.setPrintAfterEachPage(true);
        reportRequest.setReportInfo(ReportConstants.HOUSE_BILL);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.DRAFT);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.ORIGINALS, 1);
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setAdditionalDetails(new AdditionalDetails());
        shipmentDetails.getAdditionalDetails().setOriginal(1);
        shipmentDetails.getAdditionalDetails().setReleaseType("ORG");
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentDao.update(shipmentDetails, false)).thenReturn(shipmentDetails);
        Hbl hbl = new Hbl();
        hbl.setHblData(new HblDataDto());
        hbl.getHblData().setOriginalSeq(1);
        hbl.getHblData().setVersion(1);
        when(hblDao.findByShipmentId(Long.parseLong(reportRequest.getReportId()))).thenReturn(List.of(hbl));
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getHouseBillSurrenderDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setHouseMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setHouseMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        shipmentSettingsDetails2.setPrintAfterEachPage(true);
        reportRequest.setReportInfo(ReportConstants.HOUSE_BILL);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.DRAFT);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.ORIGINALS, 1);
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setAdditionalDetails(new AdditionalDetails());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getSeaShippingInstructionDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setSeaShippingInstructionMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setSeaShippingInstructionMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        shipmentSettingsDetails2.setPrintAfterEachPage(true);
        reportRequest.setReportInfo(ReportConstants.SHIPPING_INSTRUCTION);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.ORIGINALS, 1);
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        dataRetrived.put(ReportConstants.SHIPMENT_TYPE, ReportConstants.EXP);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setAdditionalDetails(new AdditionalDetails());
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShippingRequestDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setShippingRequestMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setShippingRequestMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        shipmentSettingsDetails2.setPrintAfterEachPage(true);
        reportRequest.setReportInfo(ReportConstants.SHIPPING_REQUEST);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.ORIGINALS, 1);
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        dataRetrived.put(ReportConstants.SHIPMENT_TYPE, ReportConstants.EXP);
        dataRetrived.put(ReportConstants.SHIPMENT_IDS, "1,2");
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setAdditionalDetails(new AdditionalDetails());
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getCargoManifestDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setCargoManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setCargoManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CARGO_MANIFEST);
        reportRequest.setFromConsolidation(true);
        // Mock
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(4415L);
        consolidationDetails.setShipmentsList(new HashSet<>(List.of(shipmentDetails)));
        when(consolidationDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        when(self.getDocumentData(any())).thenReturn(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf")));
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getCargoManifestAirImportDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirImportConsoleManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAirImportConsoleManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CARGO_MANIFEST_AIR_IMPORT_CONSOLIDATION);
        reportRequest.setFromConsolidation(true);
        // Mock
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(4415L);
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setDestinationPort("Test");
        shipmentDetails.setCarrierDetails(carrierDetails);
        consolidationDetails.setShipmentsList(new HashSet<>(List.of(shipmentDetails)));
        when(consolidationDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        when(self.getDocumentData(any())).thenReturn(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf")));
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getPackinListAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setPackingListMainPageAir("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setPackingListMainPageAir("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.PACKING_LIST);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getPackinListSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setPackingListMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setPackingListMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.PACKING_LIST);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipCanSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setCanMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setCanMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.SHIPMENT_CAN_DOCUMENT);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(shipmentCANReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipCanAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setCanMainPageAir("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setCanMainPageAir("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.SHIPMENT_CAN_DOCUMENT);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getAirWayBillDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirwayMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAirwayMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.AIRWAY_BILL);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipCustomSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setCustomsInsMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setCustomsInsMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CUSTOMS_INSTRUCTION);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipCustomAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setCustomsInsMainPageAir("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setCustomsInsMainPageAir("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CUSTOMS_INSTRUCTION);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipArrivalNoticeSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setArrivalNotice("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setArrivalNotice("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.ARRIVAL_NOTICE);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(arrivalNoticeReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipArrivalNoticeAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setArrivalNoticeAir("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setArrivalNoticeAir("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.ARRIVAL_NOTICE);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipFreightCertificationNoticeSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setFreightCertification("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setFreightCertification("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.FREIGHT_CERTIFICATION);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipFreightCertificationAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setFreightCertificationAir("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setFreightCertificationAir("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.FREIGHT_CERTIFICATION);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipPreAlertSeaDocumentData2()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setPreAlertDoc("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);
        shipmentSettingsDetails.setPreAlertEmailAndLogs(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setPreAlertDoc("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        shipmentSettingsDetails2.setPreAlertEmailAndLogs(true);
        reportRequest.setReportInfo(ReportConstants.PRE_ALERT);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(preAlertReport);
        when(docDetailsDao.findByEntityIdAndType(any(), any())).thenReturn(List.of(DocDetails.builder().versionNumber("2").build()));
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        Mockito.doNothing().when(eventService).saveEvent(any());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setPreAlertEmailAndLogs(true);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipPreAlertSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setPreAlertDoc("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);
        shipmentSettingsDetails.setPreAlertEmailAndLogs(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setPreAlertDoc("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        shipmentSettingsDetails2.setPreAlertEmailAndLogs(true);
        reportRequest.setReportInfo(ReportConstants.PRE_ALERT);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(preAlertReport);
        when(docDetailsDao.findByEntityIdAndType(any(), any())).thenReturn(new ArrayList<>());
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        Mockito.doNothing().when(eventService).saveEvent(any());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setPreAlertEmailAndLogs(true);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipPreAlertAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setPreAlertAir("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setPreAlertAir("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.PRE_ALERT);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipProofOfDeliveryDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setProofOfDelivery("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setProofOfDelivery("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.PROOF_OF_DELIVERY);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getPickupOrderSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setPickupOrder("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setPickupOrder("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.PICKUP_ORDER);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(pickupOrderReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }
    @Test
    void getPickupOrderV3SeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setPickupOrder("123456789");
        shipmentSettingsDetails.setTransportInstructionPickupOrder("1234ab34");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setPickupOrder("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setTransportInstructionId("123");
        reportRequest.setReportInfo(ReportConstants.PICKUP_ORDER_V3);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(transportInstructionLegsService.findByTransportInstructionId(anyLong())).thenReturn(List.of(new TiLegs()));
        when(pickupDeliveryDetailsService.findById(anyLong())).thenReturn(Optional.of(new PickupDeliveryDetails()));
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(pickupOrderReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }
    @Test
    void getPickupOrderV3SeaDocumentDataWithLegs()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setPickupOrder("123456789");
        shipmentSettingsDetails.setTransportInstructionPickupOrder("1234ab34");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setPickupOrder("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setTiLegs(Set.of(1l));
        reportRequest.setTransportInstructionId("123");
        reportRequest.setReportInfo(ReportConstants.PICKUP_ORDER_V3);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        TiLegs tiLegs = new TiLegs();
        tiLegs.setPickupDeliveryDetailsId(123l);
        when(transportInstructionLegsService.retrieveByIdIn(any())).thenReturn(List.of(tiLegs));
        when(pickupDeliveryDetailsService.findById(anyLong())).thenReturn(Optional.of(new PickupDeliveryDetails()));
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(pickupOrderReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipPicupOrderAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setPickupOrderAir("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setPickupOrderAir("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.PICKUP_ORDER);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getDeliveryOrderSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setDeliveryOrder("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setDeliveryOrder("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.DELIVERY_ORDER);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(deliveryOrderReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipDeliveryOrderAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setDeliveryOrderAir("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setDeliveryOrderAir("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.DELIVERY_ORDER);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getBookingConfirmationSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setBookingConfirmation("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setBookingConfirmation("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.BOOKING_CONFIRMATION);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(bookingConfirmationReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        verify(eventService, times(0)).saveEvent(any());
        assertNotNull(data);
    }

    @Test
    void getShipBookingConfirmationAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setBookingConfirmationAir("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setBookingConfirmationAir("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.BOOKING_CONFIRMATION);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
        verify(eventService, times(0)).saveEvent(any());
    }

    @Test
    void getCoastalDocDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setCostalDocument("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setCostalDocument("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.COSTAL_DOC);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getConsolidationPackingListDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setConsolidatedPackingList("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setConsolidatedPackingList("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CONSOLIDATED_PACKING_LIST);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShippingRequestAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setShippingRequestAir("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setShippingRequestAir("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.SHIPPING_REQUEST_AIR);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getImportShipmentManifestSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setSeaImportShipmentManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setSeaImportShipmentManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.IMPORT_SHIPMENT_MANIFEST);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipImportManifestAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirImportShipmentManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAirImportShipmentManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.IMPORT_SHIPMENT_MANIFEST);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getExportShipmentManifestSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setSeaExportShipmentManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setSeaExportShipmentManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.EXPORT_SHIPMENT_MANIFEST);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipExportManifestAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirExportShipmentManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAirExportShipmentManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.EXPORT_SHIPMENT_MANIFEST);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipCargoManifestAirImportDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirImportShipmentManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAirImportShipmentManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CARGO_MANIFEST_AIR_IMPORT_SHIPMENT);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(cargoManifestAirShipmentReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getShipCargoManifestAirExportDocumentDataSuccess()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirExportShipmentManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAirExportShipmentManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CARGO_MANIFEST_AIR_EXPORT_SHIPMENT);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);

        var mockAwb = jsonTestUtility.getTestHawb();
        mockAwb.setPrintType(PrintType.ORIGINAL_PRINTED);

        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    //    @Test
    void getShipCargoManifestAirExportDocumentDataFailsWhenOriginalAwbNotPrinted() {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirExportShipmentManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAirExportShipmentManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CARGO_MANIFEST_AIR_EXPORT_SHIPMENT);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);

        assertThrows(RunnerException.class, () -> reportService.getDocumentData(commonRequestModel));

    }

    @Test
    void getShipCargoManifestAirConsolidationDocumentData() {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirExportConsoleManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAirExportConsoleManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CARGO_MANIFEST_AIR_EXPORT_CONSOLIDATION);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock

        var mockMawb = jsonTestUtility.getTestMawb();
        mockMawb.setPrintType(PrintType.ORIGINAL_PRINTED);
        var mockHawb = jsonTestUtility.getTestHawb();

        when(awbDao.findByConsolidationId(anyLong())).thenReturn(List.of(mockMawb));
        when(awbDao.getLinkedAwbFromMawb(any())).thenReturn(Collections.singletonList(mockHawb));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        var e = assertThrows(RunnerException.class, () -> reportService.getDocumentData(commonRequestModel));
        assertNotNull(e.getMessage());
    }

    @Test
    void getShipCargoManifestAirConsolidationDocumentDataSuccess()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirExportConsoleManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAirExportConsoleManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CARGO_MANIFEST_AIR_EXPORT_CONSOLIDATION);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(cargoManifestAirConsolidationReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");

        var mockMawb = jsonTestUtility.getTestMawb();
        mockMawb.setPrintType(PrintType.ORIGINAL_PRINTED);
        var mockHawb = jsonTestUtility.getTestHawb();
        mockHawb.setPrintType(PrintType.ORIGINAL_PRINTED);
        var mockHawb1 = objectMapper.convertValue(mockHawb, Awb.class);
        mockHawb1.setPrintType(PrintType.ORIGINAL_PRINTED);

        when(awbDao.findByConsolidationId(anyLong())).thenReturn(List.of(mockMawb));
        when(awbDao.getLinkedAwbFromMawb(any())).thenReturn(Arrays.asList(mockHawb, mockHawb1));

        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getImportConsolManifestSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setSeaImportConsoleManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setSeaImportConsoleManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.IMPORT_CONSOL_MANIFEST);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        dataRetrived.put(ReportConstants.OBJECT_TYPE, ReportConstants.SEA);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getConsolImportManifestAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirImportConsoleManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAirImportConsoleManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.IMPORT_CONSOL_MANIFEST);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getExportConsolManifestSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setSeaExportConsoleManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setSeaExportConsoleManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.EXPORT_CONSOL_MANIFEST);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getConsolExportManifestAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAirExportConsoleManifest("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAirExportConsoleManifest("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.EXPORT_CONSOL_MANIFEST);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getCSRDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setCsr("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setCsr("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CSR);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getCommercialInvoiceSeaDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setCommercialInvMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setCommercialInvMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.COMMERCIAL_INVOICE);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getCommercialInvoiceAirDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setCommercialInvMainPageAir("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setCommercialInvMainPageAir("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.COMMERCIAL_INVOICE);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getIsfFileDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsfFileMainPage("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setIsfFileMainPage("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.GENERATE_ISF_FILE);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getContainerManifestDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setContainerManifestPrint("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setContainerManifestPrint("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.CONTAINER_MANIFEST_PRINT);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getManifestPrintDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setManifestPrint("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setManifestPrint("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.MANIFEST_PRINT);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getTransportOrderDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setTransportOrderRoad("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setTransportOrderRoad("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.TRANSPORT_ORDER);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        when(mawbReport.getData(any())).thenReturn(dataRetrived);
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void createDocumentTagsForShipment() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(new ShipmentDetails()));
        Mockito.doNothing().when(shipmentTagsForExteranlServices).populateRaKcDataWithShipmentDetails(any(), any());
        ResponseEntity<IRunnerResponse> responseEntity = reportService.createDocumentTagsForShipment(commonRequestModel);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createDocumentTagsForShipmentGuid() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(UUID.randomUUID().toString()).build());
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(new ShipmentDetails()));
        Mockito.doNothing().when(shipmentTagsForExteranlServices).populateRaKcDataWithShipmentDetails(any(), any());
        ResponseEntity<IRunnerResponse> responseEntity = reportService.createDocumentTagsForShipment(commonRequestModel);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void idNotExits() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().build());
        Exception e = assertThrows(RunnerException.class, () ->
                reportService.createDocumentTagsForShipment(commonRequestModel));

        String errorMessage = "Id and GUID can't be null. Please provide any one !";
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void shipmentNotExits() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(UUID.randomUUID().toString()).build());
        Exception e = assertThrows(DataRetrievalFailureException.class, () ->
                reportService.createDocumentTagsForShipment(commonRequestModel));

        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, e.getMessage());
    }

    @Test
    void getTransportInstructionPickupOrderDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setPickupOrder("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setTransportInstructionPickupOrder("123123");
        shipmentSettingsDetails.setTransportInstructionDeliveryOrder("123123");
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setPickupOrder("123456789");
        shipmentSettingsDetails2.setTransportInstructionPickupOrder("123123");
        shipmentSettingsDetails2.setTransportInstructionDeliveryOrder("123123");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setTransportInstructionId("123");
        reportRequest.setReportInfo(ReportConstants.PICKUP_ORDER);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(pickupOrderReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");

        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getTransportInstructionDeliveryOrderDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setDeliveryOrder("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);
        shipmentSettingsDetails.setTransportInstructionDeliveryOrder("123123");
        shipmentSettingsDetails.setTransportInstructionPickupOrder("123123");
        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setDeliveryOrder("123456789");
        shipmentSettingsDetails2.setTransportInstructionPickupOrder("123123");
        shipmentSettingsDetails2.setTransportInstructionDeliveryOrder("123123");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setTransportInstructionId("123");
        reportRequest.setReportInfo(ReportConstants.DELIVERY_ORDER);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(deliveryOrderReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");

        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getTransportInstructionTransportOrderDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setDeliveryOrder("122456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);
        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setDeliveryOrder("12345622789");
        shipmentSettingsDetails2.setTransportInstructionPickupOrder("12312113");
        shipmentSettingsDetails2.setTransportInstructionDeliveryOrder("12323123");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setTransportInstructionId("12323");
        reportRequest.setReportInfo(ReportConstants.DELIVERY_ORDER);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(transportOrderReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getHblReportDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setDeliveryOrder("122456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);
        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setDeliveryOrder("12345622789");
        shipmentSettingsDetails2.setTransportInstructionPickupOrder("12312113");
        shipmentSettingsDetails2.setTransportInstructionDeliveryOrder("12323123");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setTransportInstructionId("12323");
        reportRequest.setReportInfo(ReportConstants.DELIVERY_ORDER);
        reportRequest.setPrintIATAChargeCode(true);
        reportRequest.setDisplayFreightAmount(false);
        reportRequest.setDisplayOtherAmount(false);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setPrintForParties(true);
        reportRequest.setPrintingFor_str("0");
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(hblReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");

        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void test_CSDReport_shipment_throwsException() {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setDeliveryOrder("122456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);
        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setDeliveryOrder("12345622789");
        shipmentSettingsDetails2.setTransportInstructionPickupOrder("12312113");
        shipmentSettingsDetails2.setTransportInstructionDeliveryOrder("12323123");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);

        ReportRequest reportRequest1 = new ReportRequest();
        reportRequest1.setReportInfo(ReportConstants.CSD_REPORT);
        reportRequest1.setReportId("12");
        reportRequest1.setFromConsolidation(false);

        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(csdReport);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest1);
        assertThrows(ValidationException.class, () -> reportService.getDocumentData(commonRequestModel));
    }

    @Test
    void testGeneratePdfBytes_ValidInput() {

        ReportService reportService1 = spy(reportService);
        ReportRequest reportRequest1 = mock(ReportRequest.class);
        when(reportRequest1.getCopyCountForAWB()).thenReturn(2);
        when(reportRequest1.isFromConsolidation()).thenReturn(true);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, 3);
        dataRetrived.put(ReportConstants.MAWB_NUMBER, "MAWB123");

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and addBarCodeInAWBLableReport methods
        doReturn(new byte[1]).when(reportService1).getFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest1, pages, dataRetrived, pdfBytes);

        assertEquals(6, pdfBytes.size()); // 2 copies * 3 packs = 6 PDFs
    }

    @Test
    void testGeneratePdfBytes_CopyCountNull() {
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest1 = mock(ReportRequest.class);
        when(reportRequest1.getCopyCountForAWB()).thenReturn(null); // Simulate null copy count

        DocPages pages = mock(DocPages.class);
        Map<String, Object> dataRetrived = new HashMap<>();
        List<byte[]> pdfBytes = new ArrayList<>();

        ValidationException thrown = assertThrows(ValidationException.class, () ->
                reportService1.generatePdfBytes(reportRequest1, pages, dataRetrived, pdfBytes));

        assertEquals("Copy count is less than 1", thrown.getMessage());
    }

    @Test
    void testGeneratePdfBytes_MawbOrHawbNotNull() {
        ReportService reportService1 = spy(reportService);
        ReportRequest reportRequest1 = mock(ReportRequest.class);
        when(reportRequest1.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest1.isFromConsolidation()).thenReturn(false);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.TOTAL_PACKS, 1);
        dataRetrived.put(ReportConstants.MAWB_NUMBER, "MAWB123"); // Simulate MAWB_NUMBER not being null

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).getFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest1, pages, dataRetrived, pdfBytes);

        assertNull(dataRetrived.get(ReportConstants.COUNT)); // Assert the count is set correctly
    }

    @Test
    void testGeneratePdfBytes_FromConsolidation_MawbNotNull() {
        ReportService reportService1 = spy(reportService);
        ReportRequest reportRequest1 = mock(ReportRequest.class);
        when(reportRequest1.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest1.isFromConsolidation()).thenReturn(true);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, 1);
        dataRetrived.put(ReportConstants.MAWB_NUMBER, "MAWB123");

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).getFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest1, pages, dataRetrived, pdfBytes);

        assertFalse(pdfBytes.isEmpty());
        assertEquals("MAWB12300001", dataRetrived.get(ReportConstants.MAWB_NUMBER) + "00001");
    }

    @Test
    void testGeneratePdfBytes_ConsolidationTrue() {
        // Test case where reportRequest.isFromConsolidation() returns true
        ReportService reportService1 = spy(reportService);
        ReportRequest reportRequest1 = mock(ReportRequest.class);
        when(reportRequest1.getCopyCountForAWB()).thenReturn(2);
        when(reportRequest1.isFromConsolidation()).thenReturn(true);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.MAWB_NUMBER, "MAWB123");
        dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, 3);

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).getFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest1, pages, dataRetrived, pdfBytes);

        assertEquals(6, pdfBytes.size()); // 2 copies * 3 packs
    }

    @Test
    void testGeneratePdfBytes_HAWB_NotPresent() {
        // Test case where HAWB_NUMBER is null
        ReportService reportService1 = spy(reportService);
        ReportRequest reportRequest1 = mock(ReportRequest.class);
        when(reportRequest1.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest1.isFromConsolidation()).thenReturn(false);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.MAWB_NUMBER, "MAWB123");
        dataRetrived.put(ReportConstants.TOTAL_PACKS, 1);

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).getFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest1, pages, dataRetrived, pdfBytes);

        assertEquals("MAWB123", dataRetrived.get(ReportConstants.MAWB_NUMBER)); // MAWB_NUMBER is present
        assertEquals(1, pdfBytes.size());
    }

    @Test
    void testGeneratePdfBytes_MAWB_NotPresent() {
        // Test case where MAWB_NUMBER is null
        ReportService reportService1 = spy(reportService);
        ReportRequest reportRequest1 = mock(ReportRequest.class);
        when(reportRequest1.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest1.isFromConsolidation()).thenReturn(false);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.HAWB_NUMBER, "HAWB456");
        dataRetrived.put(ReportConstants.TOTAL_PACKS, 1);

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).getFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest1, pages, dataRetrived, pdfBytes);

        assertEquals("HAWB456", dataRetrived.get(ReportConstants.HAWB_NUMBER)); // HAWB_NUMBER is present
        assertEquals(1, pdfBytes.size());
    }


    @Test
    void testGeneratePdfBytes_CopyCountLessThanOne() {
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest1 = mock(ReportRequest.class);
        when(reportRequest1.getCopyCountForAWB()).thenReturn(0);

        DocPages pages = mock(DocPages.class);
        Map<String, Object> dataRetrived = new HashMap<>();
        List<byte[]> pdfBytes = new ArrayList<>();

        ValidationException thrown = assertThrows(ValidationException.class, () ->
                reportService1.generatePdfBytes(reportRequest1, pages, dataRetrived, pdfBytes));
        assertEquals("Copy count is less than 1", thrown.getMessage());
    }

    @Test
    void testGeneratePdfBytes_NullMainDocPage() {
        ReportService reportService1 = spy(reportService);
        ReportRequest reportRequest1 = mock(ReportRequest.class);
        when(reportRequest1.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest1.isFromConsolidation()).thenReturn(false);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.TOTAL_PACKS, 1);

        List<byte[]> pdfBytes = new ArrayList<>();

//        when(reportService1.executorService.submit(any(Runnable.class)))
//            .thenAnswer(invocation -> {
//                Runnable runnable = invocation.getArgument(0);
//                runnable.run(); // Execute immediately for testing
//                return CompletableFuture.completedFuture(null);
//            });
        // Mock GetFromDocumentService to return null
        doReturn(null).when(reportService1).getFromDocumentService(any(Map.class), anyString());

       assertThrows(GenericException.class, () ->
                reportService1.generatePdfBytes(reportRequest1, pages, dataRetrived, pdfBytes)
        );
    }

    @Test
    void testGeneratePdfBytes_EmptyDataRetrived() {
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest1 = mock(ReportRequest.class);
        when(reportRequest1.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest1.isFromConsolidation()).thenReturn(false);

        DocPages pages = mock(DocPages.class);

        Map<String, Object> dataRetrived = new HashMap<>();

        List<byte[]> pdfBytes = new ArrayList<>();

        ValidationException thrown = assertThrows(ValidationException.class, () ->
                reportService1.generatePdfBytes(reportRequest1, pages, dataRetrived, pdfBytes));
        assertEquals("no of pack is less than 1", thrown.getMessage());
    }

    @Test
    void testGeneratePdfBytes_MAWBNumberPresent() {
        // Test case where MAWB_NUMBER is present
        ReportService reportService1 = spy(reportService);
        ReportRequest reportRequest1 = mock(ReportRequest.class);
        when(reportRequest1.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest1.isFromConsolidation()).thenReturn(true); // Consolidation is true, so MAWB is relevant

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.MAWB_NUMBER, "MAWB123"); // MAWB_NUMBER is present
        dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, 2);

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).getFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest1, pages, dataRetrived, pdfBytes);

        // Assert that the correct mawbNumber is generated
        assertFalse(pdfBytes.isEmpty());
        assertEquals("MAWB12300001", dataRetrived.get(ReportConstants.MAWB_NUMBER) + "00001"); // pack count appended
    }

    @Test
    void testGeneratePdfBytes_MAWBNumberAbsent() {
        // Test case where MAWB_NUMBER is absent (null)
        ReportService reportService1 = spy(reportService);
        ReportRequest reportRequest1 = mock(ReportRequest.class);
        when(reportRequest1.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest1.isFromConsolidation()).thenReturn(true); // Consolidation is true, so MAWB is relevant

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, 2); // No MAWB_NUMBER in the data

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).getFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest1, pages, dataRetrived, pdfBytes);

        // Assert that the correct mawbNumber is generated
        assertFalse(pdfBytes.isEmpty());
    }

    @Test
    void testGeneratePdfBytes_Combi() {
        // Test case where reportRequest.isFromConsolidation() returns true
        ReportService reportService1 = spy(reportService);
        ReportRequest reportRequest1 = mock(ReportRequest.class);
        when(reportRequest1.getCopyCountForAWB()).thenReturn(2);
        when(reportRequest1.isFromConsolidation()).thenReturn(true);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.MAWB_NUMBER, "MAWB123");
        dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, 3);
        dataRetrived.put(ReportConstants.IS_COMBI, true);
        List<Pair<String, Integer>> map = new ArrayList<>();
        map.add(Pair.of("hawb1", 1));
        map.add(Pair.of("hawb2", 1));
        map.add(Pair.of("hawb3", 1));
        map.add(Pair.of("hawb4", 1));
        dataRetrived.put("hawbPacksMap", map);

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).getFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest1, pages, dataRetrived, pdfBytes);

        assertEquals(6, pdfBytes.size()); // 2 copies * 3 packs
    }

    @ParameterizedTest
    @CsvSource({
            "ORIGINAL, HAWB",
            "DRAFT, MAWB",
            "DRAFT, HAWB",
            "ORIGINAL, MAWB"
    })
    void addDocumentToDocumentMasterTest(String printType, String reportInfo) {
        ReportRequest newReportRequest = new ReportRequest();
        newReportRequest.setReportId("1");
        newReportRequest.setPrintType(printType);
        newReportRequest.setReportInfo(reportInfo);

        Optional<ShipmentDetails> shipmentDetails = Optional.of(ShipmentDetails.builder().build());
        when(shipmentDao.findById(Long.parseLong(newReportRequest.getReportId()))).thenReturn(shipmentDetails);

        byte[] pdfByteContent = new byte[1];
        reportService.addDocumentToDocumentMaster(newReportRequest, pdfByteContent);

        assertNotNull(shipmentDetails);
    }

    @Test
    void addDocumentToDocumentMasterTestHAWBORIGNALWithCSDPrint()
        throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ReportRequest newReportRequest = new ReportRequest();
        newReportRequest.setReportId("1");
        newReportRequest.setPrintType("ORIGINAL");
        newReportRequest.setPrintCSD(true);
        newReportRequest.setReportInfo("HAWB");

        DocUploadRequest docUploadRequest = new DocUploadRequest();
        reportService.addCSDDocumentToDocumentMaster("1", docUploadRequest, "123");
        assertNotNull(newReportRequest);
    }

    @Test
    void triggerAutomaticTransferWithHblReport_Success() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().transportMode(Constants.TRANSPORT_MODE_SEA).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().consolidationList(Set.of(consolidationDetails)).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(consolidationService).triggerAutomaticTransfer(any(), any(), any());
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        reportService.triggerAutomaticTransfer(hblReport, reportRequest);
        verify(shipmentDao, times(1)).findById(any());
        verify(consolidationService, times(1)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferWithHblReport_InvalidCase() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().transportMode(Constants.TRANSPORT_MODE_SEA).consolidationType(Constants.CONSOLIDATION_TYPE_DRT).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().consolidationList(Set.of(consolidationDetails)).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        reportService.triggerAutomaticTransfer(hblReport, reportRequest);
        verify(shipmentDao, times(1)).findById(any());
        verify(consolidationService, times(0)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferWithHblReport_InvalidCase2() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().transportMode(Constants.TRANSPORT_MODE_AIR).consolidationType(Constants.CONSOLIDATION_TYPE_DRT).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().consolidationList(Set.of(consolidationDetails)).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        reportService.triggerAutomaticTransfer(hblReport, reportRequest);
        verify(shipmentDao, times(1)).findById(any());
        verify(consolidationService, times(0)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferWithHblReport_InvalidCase_EmptyConsole() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        reportService.triggerAutomaticTransfer(hblReport, reportRequest);
        verify(shipmentDao, times(1)).findById(any());
        verify(consolidationService, times(0)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferWithHAWBReport_Success() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().transportMode(Constants.TRANSPORT_MODE_AIR).consolidationType(Constants.SHIPMENT_TYPE_STD).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().jobType(Constants.SHIPMENT_TYPE_STD).consolidationList(Set.of(consolidationDetails)).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        doNothing().when(consolidationService).triggerAutomaticTransfer(any(), any(), any());
        reportService.triggerAutomaticTransfer(hawbreport, reportRequest);
        verify(shipmentDao, times(1)).findById(any());
        verify(consolidationService, times(1)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferWithHAWBReport_InvalidCase() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().transportMode(Constants.TRANSPORT_MODE_AIR).consolidationType(Constants.CONSOLIDATION_TYPE_DRT).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().consolidationList(Set.of(consolidationDetails)).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        reportService.triggerAutomaticTransfer(hawbreport, reportRequest);
        verify(shipmentDao, times(1)).findById(any());
        verify(consolidationService, times(0)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferWithHAWBReport_InvalidCase2() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().transportMode(Constants.TRANSPORT_MODE_SEA).consolidationType(Constants.CONSOLIDATION_TYPE_DRT).build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().consolidationList(Set.of(consolidationDetails)).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        reportService.triggerAutomaticTransfer(hawbreport, reportRequest);
        verify(shipmentDao, times(1)).findById(any());
        verify(consolidationService, times(0)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferWithHAWBReport_InvalidCase_EmptyConsole() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        reportService.triggerAutomaticTransfer(hawbreport, reportRequest);
        verify(shipmentDao, times(1)).findById(any());
        verify(consolidationService, times(0)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferWithMAWB_ConsolidationReport_Success() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        reportRequest.setFromShipment(false);
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().transportMode(Constants.TRANSPORT_MODE_AIR).consolidationType(Constants.SHIPMENT_TYPE_STD).build();
        when(consolidationDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        doNothing().when(consolidationService).triggerAutomaticTransfer(any(), any(), any());
        reportService.triggerAutomaticTransfer(mawbReport, reportRequest);
        verify(consolidationDao, times(1)).findById(any());
        verify(consolidationService, times(1)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferWithMAWB_ConsolidationReport_InvalidCase() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        reportRequest.setFromShipment(false);
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().transportMode(Constants.TRANSPORT_MODE_AIR).consolidationType(Constants.CONSOLIDATION_TYPE_DRT).build();
        when(consolidationDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        reportService.triggerAutomaticTransfer(mawbReport, reportRequest);
        verify(consolidationDao, times(1)).findById(any());
        verify(consolidationService, times(0)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferWithMAWB_ConsolidationReport_InvalidCase2() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        reportRequest.setFromShipment(false);
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().transportMode(Constants.TRANSPORT_MODE_SEA).consolidationType(Constants.SHIPMENT_TYPE_STD).build();
        when(consolidationDao.findById(any())).thenReturn(Optional.of(consolidationDetails));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        reportService.triggerAutomaticTransfer(mawbReport, reportRequest);
        verify(consolidationDao, times(1)).findById(any());
        verify(consolidationService, times(0)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferWithMAWB_ShipmentReport_Success() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        reportRequest.setFromShipment(true);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().jobType(Constants.SHIPMENT_TYPE_DRT).transportMode(Constants.TRANSPORT_MODE_AIR).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentService).triggerAutomaticTransfer(any(), any(), any());
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        reportService.triggerAutomaticTransfer(mawbReport, reportRequest);
        verify(shipmentDao, times(1)).findById(any());
        verify(shipmentService, times(1)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferWithMAWB_ShipmentReport_InvalidCase() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        reportRequest.setFromShipment(true);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().jobType(Constants.SHIPMENT_TYPE_STD).transportMode(Constants.TRANSPORT_MODE_AIR).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        reportService.triggerAutomaticTransfer(mawbReport, reportRequest);
        verify(shipmentDao, times(1)).findById(any());
        verify(consolidationService, times(0)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferWithMAWB_ShipmentReport_InvalidCase2() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isAutomaticTransferEnabled(true).build());
        mockShipmentSettings();
        reportRequest.setPrintType("ORIGINAL");
        reportRequest.setFromShipment(true);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().jobType(Constants.SHIPMENT_TYPE_DRT).transportMode(Constants.TRANSPORT_MODE_SEA).build();
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        reportService.triggerAutomaticTransfer(hawbreport, reportRequest);
        verify(shipmentDao, times(1)).findById(any());
        verify(consolidationService, times(0)).triggerAutomaticTransfer(any(), any(), any());
    }

    @Test
    void getPreAlertEmailTemplateData(){
        when(shipmentDao.findById(any())).thenReturn(Optional.of(ShipmentDetails.builder().carrierDetails(CarrierDetails.builder().build()).build()));
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        assertThrows(RunnerException.class, () -> reportService.getPreAlertEmailTemplateData(1L, 2L));
    }

    @Test
    void getEmailTemplate() {
        List<EmailTemplatesRequest> emailTemplatesRequests = new ArrayList<>();
        when(iv1Service.getEmailTemplates(any())).thenReturn(V1DataResponse.builder().entities(new ArrayList<>()).build());
        reportService.getEmailTemplate(1L, emailTemplatesRequests);
        verify(iv1Service).getEmailTemplates(any());
    }

    @Test
    void testPushFileToDocumentMasterWithOutShipmentSettings() {
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(null);

        reportService.pushFileToDocumentMaster(reportRequest, new byte[1024], null);

        verify(documentManagerService, times(0)).pushSystemGeneratedDocumentToDocMaster(any(), any(), any());
    }

    @Test
    void testPushFileToDocumentMasterWithRunner3_0Disabled() {
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().build());

        reportService.pushFileToDocumentMaster(reportRequest, new byte[1024], null);

        verify(documentManagerService, times(0)).pushSystemGeneratedDocumentToDocMaster(any(), any(), any());
    }

    @Test
    void testPushFileToDocumentMasterForFCR() {
        reportRequest.setReportInfo(ReportConstants.FCR_DOCUMENT);
        reportRequest.setEntityName(Constants.SHIPMENTS_WITH_SQ_BRACKETS);
        Map<String, Object> dataRetrieved = new HashMap<>();
        dataRetrieved.put(ReportConstants.FCR_NO, "FCR_SHP00001004");
        dataRetrieved.put(ReportConstants.TRANSPORT_MODE, "AIR");
        dataRetrieved.put(ReportConstants.DIRECTION, "EXP");

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isRunnerV3Enabled(true).build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());

        reportService.pushFileToDocumentMaster(reportRequest, new byte[1024], dataRetrieved);

        verify(documentManagerService, times(0)).pushSystemGeneratedDocumentToDocMaster(any(), any(), any());
    }

    @ParameterizedTest
    @ValueSource(strings = {Constants.SHIPMENTS_WITH_SQ_BRACKETS, Constants.CONSOLIDATION, Constants.SHIPMENT})  // Runs test for both true and false cases
    void testPushFileToDocumentMasterForTO(String entityName) {
        reportRequest.setReportInfo(ReportConstants.TRANSPORT_ORDER);
        reportRequest.setEntityName(entityName);
        Map<String, Object> dataRetrieved = new HashMap<>();
        dataRetrieved.put(ReportConstants.REFERENCE_NO, "FCR_SHP00001004");
        dataRetrieved.put(ReportConstants.TRANSPORT_MODE, "AIR");
        dataRetrieved.put(ReportConstants.SHIPMENT_TYPE, "EXP");
        dataRetrieved.put(ReportConstants.DIRECTION, "EXP");

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isRunnerV3Enabled(true).build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());

        reportService.pushFileToDocumentMaster(reportRequest, new byte[1024], dataRetrieved);

        verify(documentManagerService, times(0)).pushSystemGeneratedDocumentToDocMaster(any(), any(), any());
    }

    @Test
    void testPushFileToDocumentMasterForHBL() {
        reportRequest.setReportInfo(ReportConstants.HOUSE_BILL);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setReportId("123");
        reportRequest.setEntityName(Constants.SHIPMENT);

        Map<String, Object> dataRetrieved = new HashMap<>();

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isRunnerV3Enabled(true).build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());

        reportService.pushFileToDocumentMaster(reportRequest, new byte[1024], dataRetrieved);

        verify(documentManagerService, times(0)).pushSystemGeneratedDocumentToDocMaster(any(), any(), any());
    }


    @Test
    void testPushFileToDocumentMasterForSeawayBill() {
        reportRequest.setReportInfo(ReportConstants.SEAWAY_BILL);
        reportRequest.setReportId("123");
        reportRequest.setEntityName(Constants.CONSOLIDATION);

        Map<String, Object> dataRetrieved = new HashMap<>();

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isRunnerV3Enabled(true).build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());

        reportService.pushFileToDocumentMaster(reportRequest, new byte[1024], dataRetrieved);

        verify(documentManagerService, times(0)).pushSystemGeneratedDocumentToDocMaster(any(), any(), any());
    }

    @Test
    void testPushFileToDocumentMasterForHAWB() {
        reportRequest.setReportInfo(ReportConstants.HAWB);
        reportRequest.setPrintType(ReportConstants.ORIGINAL);
        reportRequest.setReportId("123");
        reportRequest.setEntityName(Constants.SHIPMENTS_WITH_SQ_BRACKETS);

        Map<String, Object> dataRetrieved = new HashMap<>();

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isRunnerV3Enabled(true).build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());

        reportService.pushFileToDocumentMaster(reportRequest, new byte[1024], dataRetrieved);

        verify(documentManagerService, times(0)).pushSystemGeneratedDocumentToDocMaster(any(), any(), any());
    }

    @Test
    void testPushFileToDocumentMasterForMAWB() {
        reportRequest.setReportInfo(ReportConstants.MAWB);
        reportRequest.setPrintType(ReportConstants.DRAFT);
        reportRequest.setReportId("123");
        reportRequest.setEntityName(Constants.SHIPMENT);

        Map<String, Object> dataRetrieved = new HashMap<>();

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isRunnerV3Enabled(true).build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());

        reportService.pushFileToDocumentMaster(reportRequest, new byte[1024], dataRetrieved);

        verify(documentManagerService, times(0)).pushSystemGeneratedDocumentToDocMaster(any(), any(), any());
    }

    @Test
    void testPushFileToDocumentMasterWithSelfCaller() {

        reportRequest.setSelfCall(true);
        Map<String, Object> dataRetrieved = new HashMap<>();

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isRunnerV3Enabled(true).build());

        reportService.pushFileToDocumentMaster(reportRequest, new byte[1024], dataRetrieved);

        verify(documentManagerService, times(0)).pushSystemGeneratedDocumentToDocMaster(any(), any(), any());
    }


    @ParameterizedTest
    @ValueSource(strings = {Constants.SHIPMENT, Constants.CONSOLIDATION})
    void testPushFileToDocumentMasterForDefault(String entityName) {
        reportRequest.setReportInfo(ReportConstants.CSD_INFO);
        reportRequest.setReportId("123");
        reportRequest.setEntityName(entityName);

        Map<String, Object> dataRetrieved = new HashMap<>();

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isRunnerV3Enabled(true).build());

        Runnable mockRunnable = mock(Runnable.class);

        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        reportService.pushFileToDocumentMaster(reportRequest, new byte[1024], dataRetrieved);

        verify(documentManagerService, times(1)).pushSystemGeneratedDocumentToDocMaster(any(), any(), any());
    }

    @ParameterizedTest
    @ValueSource(strings = {Constants.SHIPMENTS_WITH_SQ_BRACKETS})
    void testPushFileToDocumentMasterForInvalidEntityType(String entityName) {
        reportRequest.setReportInfo(ReportConstants.CSD_INFO);
        reportRequest.setReportId("123");
        reportRequest.setEntityName(entityName);

        Map<String, Object> dataRetrieved = new HashMap<>();

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().isRunnerV3Enabled(true).build());
        Runnable mockRunnable = mock(Runnable.class);

        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        reportService.pushFileToDocumentMaster(reportRequest, new byte[1024], dataRetrieved);

        verify(documentManagerService, times(0)).pushSystemGeneratedDocumentToDocMaster(any(), any(), any());
    }

    @Test
    void getBookingOrderReportDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setBookingOrder("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setBookingOrder("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(bookingOrderReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        reportRequest.setReportInfo(ReportConstants.BOOKING_ORDER);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getBookingOrderReportDocumentInvalidData(){
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(bookingOrderReport);
        reportRequest.setReportInfo(ReportConstants.BOOKING_ORDER);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        assertThrows(ValidationException.class, () -> reportService.getDocumentData(commonRequestModel));
    }

    @Test
    void getAwbLabelReportDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAwbLable("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAwbLable("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(awbLabelReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        reportRequest.setReportInfo(ReportConstants.AWB_LABEL);
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.TOTAL_PACKS, 1);
        when(awbLabelReport.getData(any())).thenReturn(dataRetrived);
        reportRequest.setCopyCountForAWB(2);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getAwbLabelReportInvalidDocumentData() {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAwbLable("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAwbLable("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(awbLabelReport);
        reportRequest.setReportInfo(ReportConstants.AWB_LABEL);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        assertThrows(ValidationException.class, () -> reportService.getDocumentData(commonRequestModel));
    }

    @Test
    void getFcrDocumentReportDocumentData()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setFcrDocument("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setFcrDocument("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(fcrDocumentReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        reportRequest.setReportInfo(ReportConstants.FCR_DOCUMENT);
        Mockito.when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().preAlertEmailAndLogs(true).build());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getFcrDocumentReportInvalidDocumentData() throws IOException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setFcrDocument("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setFcrDocument("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(fcrDocumentReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        reportRequest.setReportInfo(ReportConstants.FCR_DOCUMENT);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        assertThrows(NullPointerException.class, () -> reportService.getDocumentData(commonRequestModel));
    }

    @Test
    void getAwbLabelReportDocumentData2()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAwbLable("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAwbLable("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.TOTAL_PACKS, 1);
        dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, 1);
        when(awbLabelReport.getData(any())).thenReturn(dataRetrived);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(awbLabelReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        reportRequest.setReportInfo(ReportConstants.AWB_LABEL);
        reportRequest.setCopyCountForAWB(2);
        reportRequest.setPrintCustomLabel(true);
        reportRequest.setTotalHawbPieces(1);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getAwbLabelReportDocumentData3()
            throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAwbLable("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAwbLable("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.TOTAL_PACKS, 1);
        dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, 1);
        when(awbLabelReport.getData(any())).thenReturn(dataRetrived);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(awbLabelReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        reportRequest.setReportInfo(ReportConstants.AWB_LABEL);
        reportRequest.setCopyCountForAWB(2);
        reportRequest.setPrintCustomLabel(true);
        reportRequest.setTotalHawbPieces(1);
        reportRequest.setCombiLabel(true);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void testAddHouseBillToRepo() {
        DocUploadRequest uploadRequest = new DocUploadRequest();
        uploadRequest.setReportId("123");
        uploadRequest.setDocType("HB");
        uploadRequest.setId(456L);

        HblDataDto hblData = new HblDataDto();
        hblData.setVersion(2);

        Hbl hbl = new Hbl();
        hbl.setHblData(hblData);

        when(hblDao.findByShipmentId(123L)).thenReturn(List.of(hbl));
        when(masterDataUtils.withMdc(any())).thenAnswer(invocation -> invocation.getArgument(0));

        byte[] document = "dummy".getBytes();

        reportService.addHouseBillToRepo(uploadRequest, "Duplicate", document, new ShipmentSettingsDetails(), "RELEASE", "shipment-guid");

        assertEquals(3, hbl.getHblData().getVersion());
        verify(hblDao).save(hbl);
    }

    @Test
    void testAddHouseBillToRepo2() {
        DocUploadRequest uploadRequest = new DocUploadRequest();
        uploadRequest.setReportId("123");
        uploadRequest.setDocType("HB");
        uploadRequest.setId(456L);

        HblDataDto hblData = new HblDataDto();
        hblData.setVersion(null);

        Hbl hbl = new Hbl();
        hbl.setHblData(hblData);

        when(hblDao.findByShipmentId(123L)).thenReturn(List.of(hbl));
        when(masterDataUtils.withMdc(any())).thenAnswer(invocation -> invocation.getArgument(0));

        byte[] document = "dummy".getBytes();

        reportService.addHouseBillToRepo(uploadRequest, "Duplicate", document, new ShipmentSettingsDetails(), "RELEASE", "shipment-guid");

        assertNull(hbl.getHblData().getVersion());
        verify(hblDao, never()).save(any());
    }

    @Test
    void testAddHouseBillToRepo3() {
        DocUploadRequest uploadRequest = new DocUploadRequest();
        uploadRequest.setReportId("123");

        Hbl hbl = new Hbl();
        hbl.setHblData(null);

        when(hblDao.findByShipmentId(123L)).thenReturn(List.of(hbl));
        when(masterDataUtils.withMdc(any())).thenAnswer(invocation -> invocation.getArgument(0));

        byte[] document = "dummy".getBytes();

        reportService.addHouseBillToRepo(uploadRequest, "Duplicate", document, new ShipmentSettingsDetails(), "RELEASE", "shipment-guid");

        verify(hblDao, never()).save(any());
    }

    @Test
    void testAddHouseBillToRepo4() {
        DocUploadRequest uploadRequest = new DocUploadRequest();
        uploadRequest.setReportId("123");

        List<Hbl> hblListWithNulls = new ArrayList<>();
        hblListWithNulls.add(null);
        hblListWithNulls.add(null);

        when(hblDao.findByShipmentId(123L)).thenReturn(hblListWithNulls);
        when(masterDataUtils.withMdc(any())).thenAnswer(invocation -> invocation.getArgument(0));

        reportService.addHouseBillToRepo(uploadRequest, "Duplicate", "doc".getBytes(), new ShipmentSettingsDetails(), "RELEASE", "shipment-guid");

        verify(hblDao, never()).save(any());
    }
    @Test
    void testPrintForPartiesAndBarcode_success() throws Exception {
        reportRequest.setPrintingFor_str("1,2");
        reportRequest.setPrintBarcode(true);
        when(docPages.getMainPageId()).thenReturn("main-page-id");
        // Sample byte arrays
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        URL resource = classLoader.getResource("test.pdf");

        if (resource == null) {
            throw new RuntimeException("PDF file not found in test resources!");
        }

        // Convert URL to Path
        Path path = Paths.get(resource.toURI());

        // Read all bytes
        byte[] sampleDoc = Files.readAllBytes(path);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(new ResponseEntity<>(sampleDoc, HttpStatus.OK));

        Map<String, Object> data = new HashMap<>();
        List<byte[]> pdfBytes = new ArrayList<>();

        byte[] result = reportService.printForPartiesAndBarcode(reportRequest, pdfBytes, "M123", data, docPages);

        assertNotNull(result);
        assertEquals(2, pdfBytes.size());
    }
    @Test
    void testPrintForPartiesAndBarcode_Failure() throws Exception {
        reportRequest.setPrintingFor_str("1,2");
        reportRequest.setPrintBarcode(true);
        when(docPages.getMainPageId()).thenReturn("main-page-id");
        // Read all bytes
        doThrow(new RuntimeException()).when(documentService).downloadDocumentTemplate(any(), any());
        Map<String, Object> data = new HashMap<>();
        List<byte[]> pdfBytes = new ArrayList<>();
        assertThrows(GenericException.class,() -> reportService.printForPartiesAndBarcode(reportRequest, pdfBytes, "M123", data, docPages));
    }
    private Runnable mockRunnable() {
        return null;
    }

    @Test
    void testPopulateConsolidationReportData_withValidData() {
        // Given - prepare test data
        Map<String, Object> dict = new HashMap<>();

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setOriginBranch(100L);
        consolidationDetails.setReceivingBranch(200L);

        TriangulationPartner triangulationPartner = new TriangulationPartner();
        triangulationPartner.setTriangulationPartner(300L);
        consolidationDetails.setTriangulationPartnerList(List.of(triangulationPartner));

        // Mock tenant models with lowercase values
        TenantModel origin = new TenantModel();
        origin.setDisplayName("origin branch");
        origin.setAddress1("origin addr1");
        origin.setAddress2("origin addr2");
        origin.setCity("origin city");
        origin.setState("origin state");
        origin.setZipPostCode("12345");
        origin.setCountry("origin country");

        TenantModel dest = new TenantModel();
        dest.setDisplayName("dest branch");

        TenantModel triang = new TenantModel();
        triang.setDisplayName("triang branch");

        // Prepare mocked tenant map
        Map<String, TenantModel> mockedTenantMap = new HashMap<>();
        mockedTenantMap.put("100", origin);
        mockedTenantMap.put("200", dest);
        mockedTenantMap.put("300", triang);

        // Mocks
        when(masterDataUtils.fetchInTenantsList(any())).thenReturn(mockedTenantMap);

        // Call method
        reportService.populateConsolidationReportData(dict, consolidationDetails);

        // Then - validate results
        List<Map<String, Object>> originBranch = (List<Map<String, Object>>) dict.get("C_OriginBranch");
        assertNotNull(originBranch);
        assertEquals("ORIGIN BRANCH", originBranch.get(0).get("C_FullName"));
        assertEquals("ORIGIN ADDR1", originBranch.get(0).get("C_Address1"));
        assertEquals("ORIGIN CITY", originBranch.get(0).get("C_City"));

        List<Map<String, Object>> destinationBranch = (List<Map<String, Object>>) dict.get("C_DestinationBranch");
        assertNotNull(destinationBranch);
        assertEquals("DEST BRANCH", destinationBranch.get(0).get("C_FullName"));

        List<Map<String, Object>> triangBranch = (List<Map<String, Object>>) dict.get("C_TriangulationBranch1");
        assertNotNull(triangBranch);
        assertEquals("TRIANG BRANCH", triangBranch.get(0).get("C_FullName"));
    }

    @Test
    void testPopulateConsolidationReportData_withBasicFields() {
        Map<String, Object> dict = new HashMap<>();
        ConsolidationDetails details = new ConsolidationDetails();
        details.setReefer(true);
        details.setHazardous(false);

        Allocations allocations = new Allocations();
        allocations.setDgContainerCount(3);
        allocations.setDgPacks(7);
        details.setAllocations(allocations);

        AchievedQuantities aq = new AchievedQuantities();
        aq.setDgPacksType("BOX");
        aq.setDgContainerCount(2);
        aq.setDgPacks(5);
        aq.setSlacCount(9);
        details.setAchievedQuantities(aq);

        details.setAdditionalTerms("Handle with care");

        reportService.populateConsolidationReportData(dict, details);

        assertEquals(true, dict.get(ReportConstants.C_D_Reefer));
        assertEquals(false, dict.get(ReportConstants.C_D_DG));
        assertEquals(3, dict.get(ReportConstants.C_CA_DGContainer));
        assertEquals(7, dict.get(ReportConstants.C_CA_DGPackages));
        assertEquals("BOX", dict.get(ReportConstants.C_C_DGPackagesType));
        assertEquals(2, dict.get(ReportConstants.C_C_DGContainer));
        assertEquals(5, dict.get(ReportConstants.C_C_DGPackages));
        assertEquals(9, dict.get(ReportConstants.C_C_SLACCount));
        assertEquals("Handle with care", dict.get(ReportConstants.C_C_AdditionalTerms));
    }

    @Test
    void testPopulateConsolidationReportData_withReferenceNumbers() {
        Map<String, Object> dict = new HashMap<>();
        ReferenceNumbers ref1 = new ReferenceNumbers();
        ref1.setType("MAWB");
        ref1.setReferenceNumber("123-456789");

        ReferenceNumbers ref2 = new ReferenceNumbers();
        ref2.setType("HAWB");
        ref2.setReferenceNumber("789-123456");

        ConsolidationDetails details = new ConsolidationDetails();
        details.setReferenceNumbersList(List.of(ref1, ref2));

        reportService.populateConsolidationReportData(dict, details);

        assertEquals("123-456789", dict.get("C_MAWB"));
        assertEquals("789-123456", dict.get("C_HAWB"));
    }

    @Test
    void testPopulateConsolidationReportData_withRoutingDetails() {
        Map<String, Object> dict = new HashMap<>();

        Routings first = new Routings();
        first.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        first.setVesselName("First Vessel");
        first.setVoyage("FV001");
        first.setCarrier("Carrier1");
        first.setFlightNumber("FL123");

        Routings last = new Routings();
        last.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        last.setVesselName("Last Vessel");
        last.setVoyage("LV001");
        last.setCarrier("Carrier2");
        last.setFlightNumber("FL999");

        ConsolidationDetails details = new ConsolidationDetails();
        details.setRoutingsList(List.of(first, last));

        reportService.populateConsolidationReportData(dict, details);

        assertEquals("First Vessel", dict.get(ReportConstants.C_FirstVessel));
        assertEquals("Carrier1", dict.get(ReportConstants.C_FirstCarrier));
        assertEquals("FL123", dict.get(ReportConstants.C_FirstFlightNumber));

        assertEquals("Last Vessel", dict.get(ReportConstants.C_LastVessel));
        assertEquals("Carrier2", dict.get(ReportConstants.C_LastCarrier));
        assertEquals("FL999", dict.get(ReportConstants.C_LastFlightNumber));
    }

    @Test
    void testPopulateConsolidationReportData_withPartiesAndAgents() {
        Map<String, Object> dict = new HashMap<>();

        Parties shipper = new Parties();
        shipper.setType("Shipper");
        Map<String, Object> orgData = Map.of(PartiesConstants.FULLNAME, "Shipper Ltd.");
        shipper.setOrgData(orgData);
        Map<String, Object> addrData = Map.of(
                PartiesConstants.ADDRESS1, "123 Street",
                PartiesConstants.CITY, "Cityville",
                PartiesConstants.COUNTRY, "India"
        );
        shipper.setAddressData(addrData);

        ConsolidationDetails details = new ConsolidationDetails();
        details.setConsolidationAddresses(List.of(shipper));
        details.setSendingAgent(shipper); // reuse as agent for test

        reportService.populateConsolidationReportData(dict, details);

        List<Map<String, Object>> shipperMapped = (List<Map<String, Object>>) dict.get("C_Shipper");
        assertNotNull(shipperMapped);
        assertEquals("123 STREET", shipperMapped.get(0).get("C_Address1"));
        assertEquals("CITYVILLE", shipperMapped.get(0).get("C_City"));
        assertEquals("INDIA", shipperMapped.get(0).get("C_Country"));

        List<Map<String, Object>> originAgent = (List<Map<String, Object>>) dict.get("C_OriginAgent");
        assertNotNull(originAgent);
        assertEquals("SHIPPER LTD.", originAgent.get(0).get("C_FullName"));
    }


}
