package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.impl.CarrierBookingDao;
import com.dpw.runner.shipment.services.dao.impl.TransactionHistoryDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShippingInstructionDao;
import com.dpw.runner.shipment.services.dao.interfaces.IVerifiedGrossMassDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.TransactionHistoryResponse;
import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class TransactionHistoryServiceTest {

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

    @BeforeEach
    void setup() {
        transactionHistoryService = new TransactionHistoryService(
                verifiedGrossMassDao,
                jsonHelper,
                carrierBookingDao,
                shippingInstructionDao,
                transactionHistoryDao
        );
    }

    @Test
    void testRetrieveByIdVGM_Success() {
        // Arrange test data
        TransactionHistory history = new TransactionHistory();
        history.setId(1L);
        history.setDescription("desc");

        TransactionHistoryResponse response = new TransactionHistoryResponse();
        response.setId(1L);
        response.setDescription("desc");
        response.setActionStatusDescription("someStatus");

        // Mock UserContext.getUser().getTenantId()
        try (MockedStatic<UserContext> userContextMockedStatic = mockStatic(UserContext.class)) {
            UsersDto mockUser = new UsersDto();
            mockUser.setTenantId(1); // Set dummy tenant ID

            userContextMockedStatic.when(UserContext::getUser).thenReturn(mockUser);

            // Mock repository calls
            when(verifiedGrossMassDao.existsById(1L)).thenReturn(true);
            when(transactionHistoryDao.findAllByEntityIdAndEntityType(1L, "VGM", 1)).thenReturn(List.of(history));
            when(jsonHelper.convertValue(history, TransactionHistoryResponse.class)).thenReturn(response);

            // Act
            List<TransactionHistoryResponse> result = transactionHistoryService.retrieveById(1L, EntityTypeTransactionHistory.VGM);

            // Assert
            assertNotNull(result);
            assertNotNull(result);
            assertEquals(1, result.size());
            assertEquals("desc", result.get(0).getDescription());
        }
    }

    @Test
    void testRetrieveByIdVGM_NoTransactionHistory() {
        // Mock tenant context
        try (MockedStatic<UserContext> userContextMockedStatic = mockStatic(UserContext.class)) {
            UsersDto mockUser = new UsersDto();
            mockUser.setTenantId(1);

            userContextMockedStatic.when(UserContext::getUser).thenReturn(mockUser);

            // Mock DAO responses
            when(verifiedGrossMassDao.existsById(1L)).thenReturn(true);
            when(transactionHistoryDao.findAllByEntityIdAndEntityType(1L, "VGM", 1)).thenReturn(Collections.emptyList());

            // Call the service
            List<TransactionHistoryResponse> result = transactionHistoryService.retrieveById(1L, EntityTypeTransactionHistory.VGM);

            assertNotNull(result);
            assertTrue(result.isEmpty());
        }
    }

    @Test
    void testRetrieveByIdVGM_NotExists() {
        when(verifiedGrossMassDao.existsById(1L)).thenReturn(false);
        assertThrows(DataRetrievalFailureException.class,
                () -> transactionHistoryService.retrieveById(1L, EntityTypeTransactionHistory.VGM));
    }

    @Test
    void testRetrieveByIdCARRIER_BOOKING_NotExists() {
        when(carrierBookingDao.existsById(2L)).thenReturn(false);
        assertThrows(DataRetrievalFailureException.class,
                () -> transactionHistoryService.retrieveById(2L, EntityTypeTransactionHistory.CARRIER_BOOKING));
    }

    @Test
    void testRetrieveByIdCARRIER_BOOKING_Success() {
        // Set up test data
        TransactionHistory history = new TransactionHistory();
        history.setId(2L);
        history.setDescription("CB Desc");

        TransactionHistoryResponse response = new TransactionHistoryResponse();
        response.setId(2L);
        response.setDescription("CB Desc");

        // Mock UserContext
        try (MockedStatic<UserContext> userContextMockedStatic = mockStatic(UserContext.class)) {
            UsersDto mockUser = new UsersDto();
            mockUser.setTenantId(1); // Dummy tenant ID

            userContextMockedStatic.when(UserContext::getUser).thenReturn(mockUser);

            // Mock DAO responses
            when(carrierBookingDao.existsById(2L)).thenReturn(true);
            when(transactionHistoryDao.findAllByEntityIdAndEntityType(2L, "CARRIER_BOOKING", 1)).thenReturn(List.of(history));
            when(jsonHelper.convertValue(history, TransactionHistoryResponse.class)).thenReturn(response);

            // Call the service
            List<TransactionHistoryResponse> result = transactionHistoryService.retrieveById(2L, EntityTypeTransactionHistory.CARRIER_BOOKING);

            // Assertions
            assertEquals(1, result.size());
            assertEquals("CB Desc", result.get(0).getDescription());
        }
    }

    @Test
    void testRetrieveById_SI_NotExists() {
        when(shippingInstructionDao.existsById(3L)).thenReturn(false);
        assertThrows(DataRetrievalFailureException.class,
                () -> transactionHistoryService.retrieveById(3L, EntityTypeTransactionHistory.SI));
    }
}
