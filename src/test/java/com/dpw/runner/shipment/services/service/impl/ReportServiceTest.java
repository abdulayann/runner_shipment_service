package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.DocumentService.DocumentService;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Reports.MawbReport;
import com.dpw.runner.shipment.services.ReportingService.Reports.SeawayBillReport;
import com.dpw.runner.shipment.services.ReportingService.ReportsFactory;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.impl.*;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.impl.DocumentManagerServiceImpl;
import com.dpw.runner.shipment.services.dto.request.ReportRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.itextpdf.text.DocumentException;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ExecutorService;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class ReportServiceTest {

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
    private MawbReport mawbReport;

    @Mock
    private DocumentService documentService;

    @Mock
    private JsonHelper jsonHelper;

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
    private FileRepoDao fileRepoDao;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private ExecutorService executorService;

    @Mock
    private DocumentManagerServiceImpl documentManagerService;

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
    }


    @Test
    void getSeawayBillDocumentData() throws DocumentException, RunnerException, IOException {
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
        Mockito.doNothing().when(eventDao).autoGenerateEvents(any());

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
    void getShipTruckWayDocumentData() throws DocumentException, RunnerException, IOException {
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
    void getConTruckWayDocumentData() throws DocumentException, RunnerException, IOException {
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
    void getShipTruckDriverDocumentData() throws DocumentException, RunnerException, IOException {
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
    void getConsTruckDriverDocumentData() throws DocumentException, RunnerException, IOException {
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
    void getAwbLableDocumentData() throws DocumentException, RunnerException, IOException {
        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setAwbLable("123456789");
        shipmentSettingsDetails.setTenantId(1);
        shipmentSettingsDetails.setAutoEventCreate(true);

        ShipmentSettingsDetails shipmentSettingsDetails2 = new ShipmentSettingsDetails();
        shipmentSettingsDetails2.setAwbLable("123456789");
        shipmentSettingsDetails2.setTenantId(44);
        shipmentSettingsDetails2.setAutoEventCreate(true);
        reportRequest.setReportInfo(ReportConstants.AWB_LABEL);
        reportRequest.setCopyCountForAWB(1);
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
    void getMAwbDocumentData() throws DocumentException, RunnerException, IOException {
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
    void getHawbDocumentData() throws DocumentException, RunnerException, IOException {
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
    void getHawbDraftDocumentData() throws DocumentException, RunnerException, IOException {
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
    void getHawbNeutralDocumentData() throws DocumentException, RunnerException, IOException {
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
    void getHouseBillDocumentData() throws DocumentException, RunnerException, IOException {
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
    void getHouseBillDraftDocumentData() throws DocumentException, RunnerException, IOException {
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

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getHouseBillSurrenderDocumentData() throws DocumentException, RunnerException, IOException {
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

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(reportRequest);
        byte[] data = reportService.getDocumentData(commonRequestModel);
        assertNotNull(data);
    }

    @Test
    void getSeaShippingInstructionDocumentData() throws DocumentException, RunnerException, IOException {
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
    void getShippingRequestDocumentData() throws DocumentException, RunnerException, IOException {
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
}
