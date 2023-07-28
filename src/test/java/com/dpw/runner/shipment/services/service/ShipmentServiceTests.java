
package com.dpw.runner.shipment.services.service;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsAspect;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.RetrieveValidateAspect;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.mapper.ShipmentDetailsMapper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.impl.ShipmentService;
import java.util.Arrays;
import java.util.Optional;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.aspectj.lang.JoinPoint;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.ResponseEntity;

@SpringBootTest
@ExtendWith(MockitoExtension.class)
public class ShipmentServiceTests {
    @Mock
    IShipmentRepository shipmentRepository;
    @Mock
    ShipmentDao shipmentDao;
    @Mock
    PermissionsContext permissionsContext;
    @Spy
    PermissionsAspect permissionsAspect;
    @Spy
    RetrieveValidateAspect retrieveValidateAspect;
    @Spy
    ModelMapper modelMapper;
    @Mock
    ShipmentDetailsMapper shipmentDetailsMapper;
    @Autowired
    ObjectMapper objectMapper;

    @InjectMocks
    private ShipmentService shipmentService;

    String retrieveErrorMessage = "Unavailable to retrieve record due to insufficient retrieve permissions";

    ShipmentDetails shipmentDetail1 = ShipmentDetails.builder()
            .status(1)
            .direction("EXP")
            .source("API")
            .transportMode("SEA")
            .shipmentType("FCL")
            .isDomestic(false)
            .houseBill("FTOGI1283602230TzrNGM")
            .masterBill("9g4e5ayd93")
            .bookingReference("0F3RY53NJH")
            .consolRef("13073QR1N5")
            .paymentTerms("C1E")
            .goodsDescription("8WM13OLH9R")
            .additionalTerms("ACJ6O7ZVX2")
            .build();

    //**************END

    ShipmentDetails shipmentDetail2 = ShipmentDetails.builder()
            .status(0)
            .direction("EXP")
            .source("API")
            .transportMode("AIR")
            .shipmentType("LCL")
            .isDomestic(false)
            .houseBill("FT0TzrNGM")
            .masterBill("12344AYD93")
            .bookingReference("0F3RY5333H")
            .consolRef("13073QR1N5")
            .paymentTerms("C1E")
            .goodsDescription("8W2323OLH9R")
            .additionalTerms("ABCD6O7ZVX2")
            .build();

    //*************END

    ShipmentDetails shipmentDetail3 = ShipmentDetails.builder()
            .status(0)
            .direction("EXP")
            .source("API")
            .transportMode("AIR")
            .shipmentType("LCL")
            .isDomestic(true)
            .houseBill("FTOGI1283602230TzrNGM")
            .masterBill("12344AYD94")
            .bookingReference("0F3RY5334H")
            .consolRef("13073QR1N6")
            .paymentTerms("C4E")
            .goodsDescription("8W2323OLH0R")
            .additionalTerms("ABCD7O7ZVX2")
            .build();

    //*************END

    ShipmentDetails shipmentDetail4 = ShipmentDetails.builder()
            .status(0)
            .direction("EXP")
            .source("Runner")
            .transportMode("AIR")
            .shipmentType("LCL")
            .isDomestic(false)
            .houseBill("FT0TzrNG4")
            .masterBill("12344AYD95")
            .bookingReference("0F3RY5335H")
            .consolRef("13073QR1N9")
            .paymentTerms("C5E")
            .goodsDescription("0W2323OLH1R")
            .additionalTerms("ABCD7O7ZVX5")
            .build();

    //****END

    ShipmentDetails shipmentDetail5 = ShipmentDetails.builder()
            .shipmentId("SHP000102015")
            .status(0)
            .direction("EXP")
            .source("API")
            .transportMode("SEA")
            .shipmentType("FCL")
            .isDomestic(true)
            .houseBill("FTOGI128360230TzrNGM")
            .masterBill("9G4E5AYD923")
            .bookingReference("0F3RY53NJH")
            .consolRef("13073QR1N5")
            .paymentTerms("C2E")
            .goodsDescription("8WM13OLH0R")
            .additionalTerms("ACJ6O7ZVX4")
            .build();

    // ************************** LIST Permission validations **********************************
    @Test
    public void exportAirShipmentListPermissionYieldsThreeRecords_isSuccess() throws Exception {

        ListCommonRequest listCommonRequest = ListCommonRequest.builder()
                .filterCriteria(Arrays.asList())
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder().fieldName("createdAt").order("asc").build())
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        PermissionsContext.setPermissions(Arrays.asList(
                "Shipments:List:Air Shipment:ExportAirShipmentList"
        ));
        Page<ShipmentDetails> shipmentDetailsPage = new PageImpl<ShipmentDetails>(Arrays.asList(
                shipmentDetail2, shipmentDetail3, shipmentDetail4));

        Mockito.when(shipmentDao.findAll(any(), any())).thenReturn(shipmentDetailsPage);

        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), commonRequestModel);
        ResponseEntity<?> shipmentResponse = shipmentService.list(commonRequestModel);
        RunnerListResponse<ShipmentDetailsResponse> response =  (RunnerListResponse<ShipmentDetailsResponse>) shipmentResponse.getBody();
        int actualCount = response.getData().size();

        // Shipments with criteria mode = AIR and direction = EXP
        assertEquals(actualCount, 3);
    }

    @Test
    public void importAirShipmentListPermissionYieldsThreeRecords_isFail() throws Exception {

        ListCommonRequest listCommonRequest = ListCommonRequest.builder()
                .filterCriteria(Arrays.asList())
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder().fieldName("createdAt").order("asc").build())
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        PermissionsContext.setPermissions(Arrays.asList(
                "Shipments:List:Air Shipment:ImportAirShipmentList"
        ));
        Page<ShipmentDetails> shipmentDetailsPage = new PageImpl<ShipmentDetails>(Arrays.asList());

        Mockito.when(shipmentDao.findAll(any(), any())).thenReturn(shipmentDetailsPage);

        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), commonRequestModel);
        ResponseEntity<?> shipmentResponse = shipmentService.list(commonRequestModel);
        RunnerListResponse<ShipmentDetailsResponse> response =  (RunnerListResponse<ShipmentDetailsResponse>) shipmentResponse.getBody();
        int actualCount = response.getData().size();

        // Shipments with criteria mode = AIR and direction = EXP
        assertEquals(3, actualCount);
    }

    @Test
    public void AllAirShipmentListPermissionYieldsAllAirShipments_isSuccess() throws Exception {

        ListCommonRequest listCommonRequest = ListCommonRequest.builder()
                .filterCriteria(Arrays.asList())
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder().fieldName("createdAt").order("asc").build())
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        PermissionsContext.setPermissions(Arrays.asList(
                "Shipments:List:Air Shipment:AllAirShipmentList"
        ));
        Page<ShipmentDetails> shipmentDetailsPage = new PageImpl<ShipmentDetails>(Arrays.asList(
                shipmentDetail2, shipmentDetail3, shipmentDetail4));

        Mockito.when(shipmentDao.findAll(any(), any())).thenReturn(shipmentDetailsPage);

        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), commonRequestModel);
        ResponseEntity<?> shipmentResponse = shipmentService.list(commonRequestModel);
        RunnerListResponse<ShipmentDetailsResponse> response =  (RunnerListResponse<ShipmentDetailsResponse>) shipmentResponse.getBody();
        int actualCount = response.getData().size();
        // Shipments with criteria mode = AIR and direction = EXP
        assertEquals(actualCount, 3);
    }

    @Test
    public void testCriteria4() throws Exception {

        ListCommonRequest listCommonRequest = ListCommonRequest.builder()
                .filterCriteria(Arrays.asList())
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder().fieldName("createdAt").order("asc").build())
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(listCommonRequest).build();

        PermissionsContext.setPermissions(Arrays.asList(
                "Shipments:List:Air Shipment:AllShipmentList"
        ));
        Page<ShipmentDetails> shipmentDetailsPage = new PageImpl<ShipmentDetails>(Arrays.asList(
                shipmentDetail1,shipmentDetail2, shipmentDetail3, shipmentDetail4,
                shipmentDetail5
        ));

        Mockito.when(shipmentDao.findAll(any(), any())).thenReturn(shipmentDetailsPage);

        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), commonRequestModel);
        ResponseEntity<?> shipmentResponse = shipmentService.list(commonRequestModel);
        RunnerListResponse<ShipmentDetailsResponse> response =  (RunnerListResponse<ShipmentDetailsResponse>) shipmentResponse.getBody();
        int actualCount = response.getData().size();

        // Shipments with criteria mode = AIR and direction = EXP
        assertEquals(actualCount, 5);
    }

    // ************************** RETRIEVE Permission validations **********************************

    @Test
    public void testCriteria6() throws Exception {

        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();

        PermissionsContext.setPermissions(Arrays.asList(
                "Shipments:Retrive:Air Shipment:ExportAirShipmentRetrive"
        ));

        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetail2));
        Mockito.when(shipmentDetailsMapper.map(any(ShipmentDetails.class))).thenReturn(
                objectMapper.convertValue(shipmentDetail2, ShipmentDetailsResponse.class)
        );
        ResponseEntity<?> shipmentResponse = shipmentService.retrieveById(commonRequestModel);
        Exception e = null;
        try {
            // Validating shipment with mode = AIR and direction = EXP
            // on ImportAirShipmentRetrive permission
            retrieveValidateAspect.validateShipmentRetrieve(Optional.of(shipmentDetail2));
        } catch (RunnerException ex) {
            e = ex;
        }

        assertNotNull(e);
        assertEquals(retrieveErrorMessage, e.getMessage());
    }

    @Test
    public void testCriteria7_shouldFail() throws Exception {

        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();

        PermissionsContext.setPermissions(Arrays.asList(
                "Shipments:Retrive:Air Shipment:ImportAirShipmentRetrive"
        ));

        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetail1));
        Mockito.when(shipmentDetailsMapper.map(any(ShipmentDetails.class))).thenReturn(
                objectMapper.convertValue(shipmentDetail2, ShipmentDetailsResponse.class)
        );
        ResponseEntity<?> shipmentResponse = shipmentService.retrieveById(commonRequestModel);
        Exception e = null;
        try {
            // Validating shipment with mode = AIR and direction = EXP
            // on ImportAirShipmentRetrive permission
            retrieveValidateAspect.validateShipmentRetrieve(Optional.of(shipmentDetail1));
        } catch (RunnerException ex) {
            e = ex;
        }


        assertNotNull(e);
        assertEquals(retrieveErrorMessage, e.getMessage());
    }

    @Test
    public void absentShipmentShouldFail() throws Exception {

        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(100L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();

        PermissionsContext.setPermissions(Arrays.asList(
                "Shipments:Retrive:Air Shipment:ImportAirShipmentRetrive",
                "Shipments:Retrive:All Shipment:AllShipmentRetrive"
        ));

        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.ofNullable(null));
        Mockito.when(shipmentDetailsMapper.map(any(ShipmentDetails.class))).thenReturn(
                objectMapper.convertValue(null, ShipmentDetailsResponse.class)
        );
        Exception e = null;
        try {
            // Validating shipment with mode = AIR and direction = EXP
            // on ImportAirShipmentRetrive permission
            ResponseEntity<?> shipmentResponse = shipmentService.retrieveById(commonRequestModel);
            retrieveValidateAspect.validateShipmentRetrieve(Optional.ofNullable(null));
        } catch (RunnerException ex) {
            e = ex;
        }

        assertNull(e);
    }

    @Test
    public void testCriteria9() throws Exception {

        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(5L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();

        PermissionsContext.setPermissions(Arrays.asList(
                "Shipments:Retrive:Sea Domestic Shipment:ExportDomesticSeaShipmentRetrive",
                "Shipments:Retrive:Sea International Shipment:DomesticSeaShipmentRetrive",
                "Shipments:Retrive:Sea International Shipment:TranshipmentSeaShipmentRetrive"
        ));

        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetail5));
        Mockito.when(shipmentDetailsMapper.map(any(ShipmentDetails.class))).thenReturn(
                objectMapper.convertValue(shipmentDetail5, ShipmentDetailsResponse.class)
        );
        ResponseEntity<?> shipmentResponse = shipmentService.retrieveById(commonRequestModel);
        Exception e = null;
        try {
            // Validating shipment with mode = SEA DOMESTIC and direction = EXP
            // on ImportAirShipmentRetrive permission
            retrieveValidateAspect.validateShipmentRetrieve(Optional.of(shipmentDetail5));
        } catch (RunnerException ex) {
            e = ex;
        }

        assertNull(e);
    }

    @Test
    public void testCriteria10() throws Exception {

        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();

        PermissionsContext.setPermissions(Arrays.asList(
                "Shipments:Retrive:Air Shipment:ImportAirShipmentRetrive"
        ));

        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetail1));
        Mockito.when(shipmentDetailsMapper.map(any(ShipmentDetails.class))).thenReturn(
                objectMapper.convertValue(shipmentDetail1, ShipmentDetailsResponse.class)
        );
        ResponseEntity<?> shipmentResponse = shipmentService.retrieveById(commonRequestModel);
        Exception e = null;
        try {
            // Validating shipment with mode = SEA and direction = EXP
            // on ImportAirShipmentRetrive permission
            retrieveValidateAspect.validateShipmentRetrieve(Optional.of(shipmentDetail1));
        } catch (RunnerException ex) {
            e = ex;
        }

        assertNotNull(e);
        assertEquals(retrieveErrorMessage, e.getMessage());
    }

}
