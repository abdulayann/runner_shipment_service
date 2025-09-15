package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dao.impl.CarrierBookingDao;
import com.dpw.runner.shipment.services.dao.impl.TransactionHistoryDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShippingInstructionDao;
import com.dpw.runner.shipment.services.dao.interfaces.IVerifiedGrossMassDao;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.TransactionHistoryResponse;
import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class TransactionHistoryServiceTest {

    @InjectMocks
    private TransactionHistoryService transactionHistoryService;

    @Mock
    private IVerifiedGrossMassDao verifiedGrossMassDao;

    @Mock
    private CarrierBookingDao carrierBookingDao;

    @Mock
    private IShippingInstructionDao shippingInstructionDao;

    @Mock
    private TransactionHistoryDao transactionHistoryDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private CommonUtils commonUtils;

    @BeforeEach
    void setUp() {
        // Ensure @PostConstruct is manually invoked
        transactionHistoryService = new TransactionHistoryService(
                verifiedGrossMassDao, jsonHelper,
                carrierBookingDao, shippingInstructionDao,
                commonUtils, transactionHistoryDao);
        transactionHistoryService.initStatusResolvers();
    }

    @Test
    void testRetrieveById_VGM_EntityFound_HistoryFound() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setStatus(VerifiedGrossMassStatus.ConfirmedByCarrier);

        TransactionHistory history = new TransactionHistory();
        history.setId(1L);
        history.setDescription("Test VGM Desc");

        TransactionHistoryResponse responseMock = new TransactionHistoryResponse();
        responseMock.setId(1L);
        responseMock.setDescription("Test VGM Desc");
        responseMock.setActionStatusDescription("Confirmed By Carrier");

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(vgm));
        when(transactionHistoryDao.findAllByEntityIdAndEntityType(1L, EntityTypeTransactionHistory.VGM.name())).thenReturn(List.of(history));
        when(jsonHelper.convertValue(eq(history), eq(TransactionHistoryResponse.class))).thenReturn(responseMock);

        ResponseEntity<IRunnerResponse> response = transactionHistoryService.retrieveById(1L, EntityTypeTransactionHistory.VGM);

        assertNotNull(response);
        RunnerListResponse body = (RunnerListResponse) response.getBody();
        assertNotNull(body);
        assertEquals(1, body.getData().size());
        TransactionHistoryResponse actual = (TransactionHistoryResponse) body.getData().get(0);
        assertEquals("Confirmed By Carrier", actual.getActionStatusDescription());
    }

    @Test
    void testRetrieveById_VGM_EntityFound_HistoryEmpty() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setStatus(VerifiedGrossMassStatus.ConfirmedByCarrier);

        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.of(vgm));
        when(transactionHistoryDao.findAllByEntityIdAndEntityType(1L, EntityTypeTransactionHistory.VGM.name())).thenReturn(Collections.emptyList());

        ResponseEntity<IRunnerResponse> response = transactionHistoryService.retrieveById(1L, EntityTypeTransactionHistory.VGM);

        assertNotNull(response);
        RunnerListResponse body = (RunnerListResponse) response.getBody();
        assertNotNull(body);
        assertTrue(body.getData().isEmpty());
    }

    @Test
    void testRetrieveById_VGM_EntityNotFound_ShouldThrowException() {
        when(verifiedGrossMassDao.findById(1L)).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class,
                () -> transactionHistoryService.retrieveById(1L, EntityTypeTransactionHistory.VGM));
    }

    @Test
    void testRetrieveById_CARRIER_BOOKING_EntityFound() {
        var cb = new com.dpw.runner.shipment.services.entity.CarrierBooking();
        cb.setStatus(CarrierBookingStatus.Requested);

        TransactionHistory history = new TransactionHistory();
        history.setId(2L);
        history.setDescription("CB Desc");

        TransactionHistoryResponse mockResponse = new TransactionHistoryResponse();
        mockResponse.setId(2L);
        mockResponse.setDescription("CB Desc");
        mockResponse.setActionStatusDescription("Booking Requested");

        when(carrierBookingDao.findById(2L)).thenReturn(Optional.of(cb));
        when(transactionHistoryDao.findAllByEntityIdAndEntityType(2L, EntityTypeTransactionHistory.CARRIER_BOOKING.name())).thenReturn(List.of(history));
        when(jsonHelper.convertValue(eq(history), eq(TransactionHistoryResponse.class))).thenReturn(mockResponse);

        ResponseEntity<IRunnerResponse> response = transactionHistoryService.retrieveById(2L, EntityTypeTransactionHistory.CARRIER_BOOKING);

        assertNotNull(response);
        RunnerListResponse body = (RunnerListResponse) response.getBody();
        assertEquals("Booking Requested", ((TransactionHistoryResponse) body.getData().get(0)).getActionStatusDescription());
    }

    @Test
    void testRetrieveById_CARRIER_BOOKING_EntityNotFound() {
        when(carrierBookingDao.findById(99L)).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class,
                () -> transactionHistoryService.retrieveById(99L, EntityTypeTransactionHistory.CARRIER_BOOKING));
    }

    @Test
    void testRetrieveById_SI_EntityFound() {
        var si = new com.dpw.runner.shipment.services.entity.ShippingInstruction();
        si.setStatus(ShippingInstructionStatus.SIAccepted);

        TransactionHistory history = new TransactionHistory();
        history.setId(3L);
        history.setDescription("SI Desc");

        TransactionHistoryResponse mockResponse = new TransactionHistoryResponse();
        mockResponse.setId(3L);
        mockResponse.setDescription("SI Desc");
        mockResponse.setActionStatusDescription("SI accepted");

        when(shippingInstructionDao.findById(3L)).thenReturn(Optional.of(si));
        when(transactionHistoryDao.findAllByEntityIdAndEntityType(3L, EntityTypeTransactionHistory.SI.name())).thenReturn(List.of(history));
        when(jsonHelper.convertValue(eq(history), eq(TransactionHistoryResponse.class))).thenReturn(mockResponse);

        ResponseEntity<IRunnerResponse> response = transactionHistoryService.retrieveById(3L, EntityTypeTransactionHistory.SI);

        assertNotNull(response);
        RunnerListResponse body = (RunnerListResponse) response.getBody();
        assertEquals("SI accepted", ((TransactionHistoryResponse) body.getData().get(0)).getActionStatusDescription());
    }

    @Test
    void testRetrieveById_SI_EntityNotFound_ShouldThrow() {
        when(shippingInstructionDao.findById(3L)).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class,
                () -> transactionHistoryService.retrieveById(3L, EntityTypeTransactionHistory.SI));
    }

    @Test
    void testRetrieveById_UnsupportedEntityType_ShouldThrowIllegalArgumentException() {
        assertThrows(IllegalArgumentException.class, () ->
                transactionHistoryService.retrieveById(1L, null));
    }
}
