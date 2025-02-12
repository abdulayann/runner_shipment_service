package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.DocumentService.DocumentService;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.DocPages;
import com.dpw.runner.shipment.services.ReportingService.Models.DocUploadRequest;
import com.dpw.runner.shipment.services.ReportingService.Reports.ArrivalNoticeReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.BookingConfirmationReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.CSDReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.CargoManifestAirConsolidationReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.CargoManifestAirShipmentReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.DeliveryOrderReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.HblReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.MawbReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.PickupOrderReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.PreAlertReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.SeawayBillReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.ShipmentCANReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.ShipmentTagsForExteranlServices;
import com.dpw.runner.shipment.services.ReportingService.Reports.TransportOrderReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.*;
import com.dpw.runner.shipment.services.ReportingService.ReportsFactory;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.impl.*;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.impl.DocumentManagerServiceImpl;
import com.dpw.runner.shipment.services.dto.request.ReportRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.PrintType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.itextpdf.text.DocumentException;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ReportServiceTest {

    @InjectMocks
    private ReportService reportService;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private ReportsFactory reportsFactory;

    @Mock
    private SeawayBillReport seawayBillReport;

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

    private ExecutorService executorService = Executors.newFixedThreadPool(10);

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
    private CSDReport csdReport;

    @Mock
    private DependentServiceHelper dependentServiceHelper;

    private Map<String, Object> dataRetrived;

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
        dataRetrived = new HashMap<>();
        reportService.executorService = executorService;
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
        // Mockito.doNothing().when(eventDao).generateEvents(any());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void invalidTemplate() throws DocumentException, RunnerException, IOException {
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

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);

        Exception e = assertThrows(ValidationException.class, () -> {
            reportService.getDocumentData(commonRequestModel);
        });

        String errorMessage ="Please Upload Valid Template";
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void invalidTemplateId() throws DocumentException, RunnerException, IOException {
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

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);

        Exception e = assertThrows(ValidationException.class, () -> {
            reportService.getDocumentData(commonRequestModel);
        });

        String errorMessage ="Please Upload Valid Template";
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void templateNotExists() throws DocumentException, RunnerException, IOException {
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

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);

        Exception e = assertThrows(ValidationException.class, () -> {
            reportService.getDocumentData(commonRequestModel);
        });

        String errorMessage ="Please upload template in branch settings for: SeawayBill";
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
        reportRequest.setReportInfo(ReportConstants.CONS_TRUCKWAY_BIll);
        // Mock
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(seawayBillReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");

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
        when(awbDao.updateAirMessageStatusFromShipmentId(any(), any())).thenReturn(1);
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
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(mawbReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(awbDao.updateAirMessageStatusFromShipmentId(any(), any())).thenReturn(1);
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
        //when(documentManagerService.saveFile(any())).thenReturn(documentManagerResponse);


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

        //when(documentManagerService.saveFile(any())).thenReturn(documentManagerResponse);


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

        //when(documentManagerService.saveFile(any())).thenReturn(documentManagerResponse);


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
//        Mockito.doNothing().when(eventService).saveEvent(any());
        Hbl hbl = new Hbl();
        hbl.setHblData(new HblDataDto());
        hbl.getHblData().setOriginalSeq(1);
        hbl.getHblData().setVersion(1);
        when(hblDao.findByShipmentId(Long.parseLong(reportRequest.getReportId()))).thenReturn(Arrays.asList(hbl));

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
//        Mockito.doNothing().when(eventService).saveEvent(any());

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
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(seawayBillReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(4415L);
        consolidationDetails.setShipmentsList(new HashSet<>(Arrays.asList(shipmentDetails)));
        when(consolidationDao.findById(any())).thenReturn(Optional.of(consolidationDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
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
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(seawayBillReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(4415L);
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setDestinationPort("Test");
        shipmentDetails.setCarrierDetails(carrierDetails);
        consolidationDetails.setShipmentsList(new HashSet<>(Arrays.asList(shipmentDetails)));
        when(consolidationDao.findById(any())).thenReturn(Optional.of(consolidationDetails));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
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
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);

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
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
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

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setPreAlertDoc("123456789");
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
        when(reportsFactory.getReport(any())).thenReturn(preAlertReport);
        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);
        Mockito.doNothing().when(eventService).saveEvent(any());

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
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);

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
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);

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
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);

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
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);

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

        when(awbDao.findByShipmentId(anyLong())).thenReturn(List.of(mockAwb));

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

//    @Test
    void getShipCargoManifestAirExportDocumentDataFailsWhenOriginalAwbNotPrinted() throws IOException {
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
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);

        assertThrows(RunnerException.class, () -> reportService.getDocumentData(commonRequestModel));

    }

    @Test
    void getShipCargoManifestAirConsolidationDocumentData() throws DocumentException, RunnerException, IOException {
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
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);

        var mockMawb = jsonTestUtility.getTestMawb();
        mockMawb.setPrintType(PrintType.ORIGINAL_PRINTED);
        var mockHawb = jsonTestUtility.getTestHawb();

        when(awbDao.findByConsolidationId(anyLong())).thenReturn(List.of(mockMawb));
        when(awbDao.getLinkedAwbFromMawb(any())).thenReturn(Arrays.asList(mockHawb));

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
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.TRANS_AIR);

        var mockMawb = jsonTestUtility.getTestMawb();
        mockMawb.setPrintType(PrintType.ORIGINAL_PRINTED);
        var mockHawb = jsonTestUtility.getTestHawb();
        mockHawb.setPrintType(PrintType.ORIGINAL_PRINTED);
        var mockHawb1 = objectMapper.convertValue(mockHawb, Awb.class);
        mockHawb1.setPrintType(PrintType.ORIGINAL_PRINTED);

        when(awbDao.findByConsolidationId(anyLong())).thenReturn(List.of(mockMawb));
        when(awbDao.getLinkedAwbFromMawb(any())).thenReturn(Arrays.asList(mockHawb, mockHawb1));


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

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void createDocumentTagsForShipment() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build());
        when(shipmentDao.findById(any())).thenReturn(Optional.of(new ShipmentDetails()));
        Mockito.doNothing().when(shipmentTagsForExteranlServices).populateRaKcData(any(), any());
        reportService.createDocumentTagsForShipment(commonRequestModel);
    }

    @Test
    void createDocumentTagsForShipmentGuid() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(UUID.randomUUID().toString()).build());
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(new ShipmentDetails()));
        Mockito.doNothing().when(shipmentTagsForExteranlServices).populateRaKcData(any(), any());
        reportService.createDocumentTagsForShipment(commonRequestModel);
    }

    @Test
    void idNotExits() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().build());
        Exception e = assertThrows(RunnerException.class, () -> {
            reportService.createDocumentTagsForShipment(commonRequestModel);
        });

        String errorMessage ="Id and GUID can't be null. Please provide any one !";
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void shipmentNotExits() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(UUID.randomUUID().toString()).build());
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> {
            reportService.createDocumentTagsForShipment(commonRequestModel);
        });

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
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);

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
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.SEA);

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
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123334");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.ROAD);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getHblReportocumentData()
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
        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.OTHER_AMOUNT_TEXT, "123334");
        dataRetrived.put(ReportConstants.TRANSPORT_MODE, ReportConstants.ROAD);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void test_CSDReport_shipment_throwsException() throws DocumentException, RunnerException, IOException {
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

        ReportRequest reportRequest = new ReportRequest();
        reportRequest.setReportInfo(ReportConstants.CSD_REPORT);
        reportRequest.setReportId("12");
        reportRequest.setFromConsolidation(false);

        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(Arrays.asList(shipmentSettingsDetails, shipmentSettingsDetails2));
        when(reportsFactory.getReport(any())).thenReturn(csdReport);
//        when(documentService.downloadDocumentTemplate(any(), any())).thenReturn(ResponseEntity.ok(Files.readAllBytes(Paths.get(path + "SeawayBill.pdf"))));
//        when(jsonHelper.convertToJson(any())).thenReturn("");

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        assertThrows(ValidationException.class , () -> reportService.getDocumentData(commonRequestModel));
    }

    @Test
    void testGeneratePdfBytes_ValidInput() {

        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest = mock(ReportRequest.class);
        when(reportRequest.getCopyCountForAWB()).thenReturn(2);
        when(reportRequest.isFromConsolidation()).thenReturn(true);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, 3);
        dataRetrived.put(ReportConstants.MAWB_NUMBER, "MAWB123");

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and addBarCodeInAWBLableReport methods
        doReturn(new byte[1]).when(reportService1).GetFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);

        assertEquals(6, pdfBytes.size()); // 2 copies * 3 packs = 6 PDFs
    }

    @Test
    void testGeneratePdfBytes_CopyCountNull() {
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest = mock(ReportRequest.class);
        when(reportRequest.getCopyCountForAWB()).thenReturn(null); // Simulate null copy count

        DocPages pages = mock(DocPages.class);
        Map<String, Object> dataRetrived = new HashMap<>();
        List<byte[]> pdfBytes = new ArrayList<>();

        ValidationException thrown = assertThrows(ValidationException.class, () -> {
            reportService1.generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);
        });

        assertEquals("Copy count is less than 1", thrown.getMessage());
    }

    @Test
    void testGeneratePdfBytes_MawbOrHawbNotNull() {
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest = mock(ReportRequest.class);
        when(reportRequest.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest.isFromConsolidation()).thenReturn(false);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.TOTAL_PACKS, 1);
        dataRetrived.put(ReportConstants.MAWB_NUMBER, "MAWB123"); // Simulate MAWB_NUMBER not being null

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).GetFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);

        assertEquals("00001", dataRetrived.get(ReportConstants.COUNT)); // Assert the count is set correctly
    }

    @Test
    void testGeneratePdfBytes_FromConsolidation_MawbNotNull() {
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest = mock(ReportRequest.class);
        when(reportRequest.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest.isFromConsolidation()).thenReturn(true);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, 1);
        dataRetrived.put(ReportConstants.MAWB_NUMBER, "MAWB123");

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).GetFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);

        assertTrue(pdfBytes.size() > 0);
        assertEquals("MAWB12300001", dataRetrived.get(ReportConstants.MAWB_NUMBER) + "00001");
    }

    @Test
    void testGeneratePdfBytes_ConsolidationTrue() {
        // Test case where reportRequest.isFromConsolidation() returns true
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest = mock(ReportRequest.class);
        when(reportRequest.getCopyCountForAWB()).thenReturn(2);
        when(reportRequest.isFromConsolidation()).thenReturn(true);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.MAWB_NUMBER, "MAWB123");
        dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, 3);

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).GetFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);

        assertEquals(6, pdfBytes.size()); // 2 copies * 3 packs
    }

    @Test
    void testGeneratePdfBytes_HAWB_NotPresent() {
        // Test case where HAWB_NUMBER is null
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest = mock(ReportRequest.class);
        when(reportRequest.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest.isFromConsolidation()).thenReturn(false);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.MAWB_NUMBER, "MAWB123");
        dataRetrived.put(ReportConstants.TOTAL_PACKS, 1);

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).GetFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);

        assertEquals("MAWB123", dataRetrived.get(ReportConstants.MAWB_NUMBER)); // MAWB_NUMBER is present
        assertEquals(1, pdfBytes.size());
    }

    @Test
    void testGeneratePdfBytes_MAWB_NotPresent() {
        // Test case where MAWB_NUMBER is null
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest = mock(ReportRequest.class);
        when(reportRequest.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest.isFromConsolidation()).thenReturn(false);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.HAWB_NUMBER, "HAWB456");
        dataRetrived.put(ReportConstants.TOTAL_PACKS, 1);

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).GetFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);

        assertEquals("HAWB456", dataRetrived.get(ReportConstants.HAWB_NUMBER)); // HAWB_NUMBER is present
        assertEquals(1, pdfBytes.size());
    }



    @Test
    void testGeneratePdfBytes_CopyCountLessThanOne() {
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest = mock(ReportRequest.class);
        when(reportRequest.getCopyCountForAWB()).thenReturn(0);

        DocPages pages = mock(DocPages.class);
        Map<String, Object> dataRetrived = new HashMap<>();
        List<byte[]> pdfBytes = new ArrayList<>();

        ValidationException thrown = assertThrows(ValidationException.class, () -> {
            reportService1.generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);
        });
        assertEquals("Copy count is less than 1", thrown.getMessage());
    }

    @Test
    void testGeneratePdfBytes_NullMainDocPage() {
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest = mock(ReportRequest.class);
        when(reportRequest.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest.isFromConsolidation()).thenReturn(false);

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.TOTAL_PACKS, 1);

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService to return null
        doReturn(null).when(reportService1).GetFromDocumentService(any(Map.class), anyString());

        ValidationException thrown = assertThrows(ValidationException.class, () -> {
            reportService1.generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);
        });
        assertEquals(ReportConstants.PLEASE_UPLOAD_VALID_TEMPLATE, thrown.getMessage());
    }

    @Test
    void testGeneratePdfBytes_EmptyDataRetrived() {
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest = mock(ReportRequest.class);
        when(reportRequest.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest.isFromConsolidation()).thenReturn(false);

        DocPages pages = mock(DocPages.class);

        Map<String, Object> dataRetrived = new HashMap<>();

        List<byte[]> pdfBytes = new ArrayList<>();

        ValidationException thrown = assertThrows(ValidationException.class, () -> {
            reportService1.generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);
        });
        assertEquals("no of pack is less than 1", thrown.getMessage());
    }

    @Test
    void testGeneratePdfBytes_MAWBNumberPresent() {
        // Test case where MAWB_NUMBER is present
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest = mock(ReportRequest.class);
        when(reportRequest.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest.isFromConsolidation()).thenReturn(true); // Consolidation is true, so MAWB is relevant

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.MAWB_NUMBER, "MAWB123"); // MAWB_NUMBER is present
        dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, 2);

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).GetFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);

        // Assert that the correct mawbNumber is generated
        assertTrue(pdfBytes.size() > 0);
        assertEquals("MAWB12300001", dataRetrived.get(ReportConstants.MAWB_NUMBER) + "00001"); // pack count appended
    }

    @Test
    void testGeneratePdfBytes_MAWBNumberAbsent() {
        // Test case where MAWB_NUMBER is absent (null)
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest = mock(ReportRequest.class);
        when(reportRequest.getCopyCountForAWB()).thenReturn(1);
        when(reportRequest.isFromConsolidation()).thenReturn(true); // Consolidation is true, so MAWB is relevant

        DocPages pages = mock(DocPages.class);
        when(pages.getMainPageId()).thenReturn("mainPageId");

        Map<String, Object> dataRetrived = new HashMap<>();
        dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, 2); // No MAWB_NUMBER in the data

        List<byte[]> pdfBytes = new ArrayList<>();

        // Mock GetFromDocumentService and other methods
        doReturn(new byte[1]).when(reportService1).GetFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);

        // Assert that the correct mawbNumber is generated
        assertTrue(pdfBytes.size() > 0);
    }

    @Test
    void testGeneratePdfBytes_Combi() {
        // Test case where reportRequest.isFromConsolidation() returns true
        ReportService reportService1 = spy(new ReportService());
        ReportRequest reportRequest = mock(ReportRequest.class);
        when(reportRequest.getCopyCountForAWB()).thenReturn(2);
        when(reportRequest.isFromConsolidation()).thenReturn(true);

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
        doReturn(new byte[1]).when(reportService1).GetFromDocumentService(any(Map.class), anyString());
        doReturn(new byte[1]).when(reportService1).addBarCodeInAWBLableReport(any(byte[].class), anyString(), anyString());

        reportService1.generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);

        assertEquals(6, pdfBytes.size()); // 2 copies * 3 packs
    }

    @Test
    void addDocumentToDocumentMasterTestHAWBORIGNAL(){
        ReportRequest reportRequest = new ReportRequest();
        reportRequest.setReportId("1");
        reportRequest.setPrintType("ORIGINAL");
        reportRequest.setReportInfo("HAWB");
        Optional<ShipmentDetails> shipmentDetails = Optional.of(ShipmentDetails.builder().build());
        when(shipmentDao.findById(Long.parseLong(reportRequest.getReportId()))).thenReturn(shipmentDetails);

        byte[] pdfByte_Content = new byte[1];
        reportService.addDocumentToDocumentMaster(reportRequest, pdfByte_Content);
        assertNotNull(shipmentDetails);
    }

    @Test
    void addDocumentToDocumentMasterTestHAWBORIGNALWithCSDPrint()
        throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ReportRequest reportRequest = new ReportRequest();
        reportRequest.setReportId("1");
        reportRequest.setPrintType("ORIGINAL");
        reportRequest.setPrintCSD(true);
        reportRequest.setReportInfo("HAWB");

        DocUploadRequest docUploadRequest = new DocUploadRequest();
        reportService.addCSDDocumentToDocumentMaster("1", docUploadRequest, "123");
        assertNotNull(reportRequest);
    }

    @Test
    void addDocumentToDocumentMasterTestMAWBDRAFT(){
        ReportRequest reportRequest = new ReportRequest();
        reportRequest.setReportId("1");
        reportRequest.setPrintType("DRAFT");
        reportRequest.setReportInfo("MAWB");
        Optional<ShipmentDetails> shipmentDetails = Optional.of(ShipmentDetails.builder().build());
        when(shipmentDao.findById(Long.parseLong(reportRequest.getReportId()))).thenReturn(shipmentDetails);

        byte[] pdfByte_Content = new byte[1];
        reportService.addDocumentToDocumentMaster(reportRequest, pdfByte_Content);
        assertNotNull(shipmentDetails);
    }

    @Test
    void addDocumentToDocumentMasterTestHAWBDRAFT(){
        ReportRequest reportRequest = new ReportRequest();
        reportRequest.setReportId("1");
        reportRequest.setPrintType("DRAFT");
        reportRequest.setReportInfo("HAWB");
        Optional<ShipmentDetails> shipmentDetails = Optional.of(ShipmentDetails.builder().build());
        when(shipmentDao.findById(Long.parseLong(reportRequest.getReportId()))).thenReturn(shipmentDetails);

        byte[] pdfByte_Content = new byte[1];
        reportService.addDocumentToDocumentMaster(reportRequest, pdfByte_Content);
        assertNotNull(shipmentDetails);
    }

    @Test
    void addDocumentToDocumentMasterTestMAWBORIGNAL(){
        ReportRequest reportRequest = new ReportRequest();
        reportRequest.setReportId("1");
        reportRequest.setPrintType("ORIGINAL");
        reportRequest.setReportInfo("MAWB");
        Optional<ShipmentDetails> shipmentDetails = Optional.of(ShipmentDetails.builder().build());
        when(shipmentDao.findById(Long.parseLong(reportRequest.getReportId()))).thenReturn(shipmentDetails);

        byte[] pdfByte_Content = new byte[1];
        reportService.addDocumentToDocumentMaster(reportRequest, pdfByte_Content);
        assertNotNull(shipmentDetails);
    }

}
