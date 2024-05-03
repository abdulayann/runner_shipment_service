package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.DocumentService.DocumentService;
import com.dpw.runner.shipment.services.ReportingService.Reports.SeawayBillReport;
import com.dpw.runner.shipment.services.ReportingService.ReportsFactory;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.impl.EventDao;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.ReportRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
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
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
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
    private DocumentService documentService;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ShipmentDao shipmentDao;

    @Mock
    private EventDao eventDao;

    private String path = "src/test/java/com/dpw/runner/shipment/services/files/";

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
}
