package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ICustomerBookingRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.dpw.runner.shipment.services.validator.custom.validations.CustomerBookingValidations;
import com.nimbusds.jose.util.Pair;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CustomerBookingDaoTest {
    private static JsonTestUtility jsonTestUtility;

    @InjectMocks
    private CustomerBookingDao customerBookingDao;

    @Mock
    private ICustomerBookingRepository customerBookingRepository;
    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private CustomerBookingValidations customValidations;

    @Mock
    private CustomerBooking customerBookingMock;

    @BeforeAll
    static void beforeAll() throws IOException {
        jsonTestUtility = new JsonTestUtility();
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(new HashMap<>()).build()); // Set up a mock user for testing
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    @Test
    void save() {
        Parties mockParty = Parties.builder().orgCode("ORG123").addressCode("ADDR123").build();
        Parties mockParty2 = Parties.builder().orgCode("ORG323").addressCode("ADER123").build();
        Parties mockParty3 = Parties.builder().orgCode("ORG325").addressCode("ADEE123").build();

        CarrierDetails carrierDetails = CarrierDetails.builder().origin("origin").destination("destination")
                .originPort("origninPort").destinationPort("destinationPort").build();

        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setBookingDate(LocalDateTime.now());
        customerBooking.setBookingNumber("BKN33123");
        customerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        customerBooking.setTransportType(Constants.TRANSPORT_MODE_SEA);
        customerBooking.setServiceMode("service mode");
        customerBooking.setBookingCharges(List.of(new BookingCharges()));
        customerBooking.setCustomer(mockParty);
        customerBooking.setConsignee(mockParty2);
        customerBooking.setConsignor(mockParty3);
        customerBooking.setIncoTerms("Inco Terms");
        customerBooking.setCargoType(Constants.CARGO_TYPE_FCL);
        customerBooking.setCarrierDetails(carrierDetails);

        when(customerBookingRepository.save(any())).thenReturn(customerBooking);
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());

        // Test
        CustomerBooking savedCustomerBooking = customerBookingDao.save(customerBooking);

        // Assert
        assertEquals(customerBooking, savedCustomerBooking);
    }

    // create tc for save by using input that fails all validations one by one to inc coverage
    @Test
    void findAll() {
        CustomerBooking testData = CustomerBooking.builder().build();

        List<CustomerBooking> customerBookingList = new ArrayList<>();
        customerBookingList.add(testData);

        PageImpl<CustomerBooking> customerBookingPage = new PageImpl<>(customerBookingList);
        ListCommonRequest listReq = constructListCommonRequest("id", 1, "=");
        Pair<Specification<CustomerBooking>, Pageable> pair = fetchData(listReq, CustomerBooking.class);

        when(customerBookingRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(customerBookingPage);
        assertEquals(customerBookingPage, customerBookingDao.findAll(pair.getLeft(), pair.getRight()));
    }

    @Test
    void findById() {
        CustomerBooking testData = CustomerBooking.builder().build();
        when(customerBookingRepository.findById(any())).thenReturn(Optional.of(testData));
        assertEquals(testData, customerBookingDao.findById(1L).get());
    }

    @Test
    void delete() {
        CustomerBooking testData = CustomerBooking.builder().build();
        customerBookingDao.delete(testData);
        verify(customerBookingRepository, times(1)).delete(testData);
    }

    @Test
    void updateIsPlatformBookingCreated() {
        when(customerBookingRepository.updateIsPlatformBookingCreated(any(), any())).thenReturn(1);
        assertEquals(1, customerBookingDao.updateIsPlatformBookingCreated(1L, true));
    }

    @Test
    void updateBillStatus() {
        when(customerBookingRepository.updateBillingStatus(any(), any())).thenReturn(1);
        assertEquals(1, customerBookingDao.updateBillStatus(1L, true));
    }

    @Test
    void SaveError() {
        HashSet<String> error = new HashSet<>();
        error.add("An Error Occured");
        CustomerBooking customerBooking = CustomerBooking.builder().build();
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(error);
        assertThrows(ValidationException.class, () -> {
            customerBookingDao.save(customerBooking);
        });
    }

    @Test
    void SaveEntityNotPresent() {
        HashSet<String> error = new HashSet<>();

        CustomerBooking customerBooking = CustomerBooking.builder().build();
        customerBooking.setId(1L);

        when(customerBookingRepository.findById(any())).thenReturn(Optional.empty());
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(error);
        assertThrows(DataRetrievalFailureException.class, () -> {
            customerBookingDao.save(customerBooking);
        });
    }

    @Test
    void SaveEntityIdNull() {
        HashSet<String> error = new HashSet<>();

        CustomerBooking customerBooking = CustomerBooking.builder().build();

        when(customerBookingRepository.findByBookingNumber(any())).thenReturn(Optional.of(customerBooking));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(error);
        assertThrows(ValidationException.class, () -> {
            customerBookingDao.save(customerBooking);
        });
    }

    @Test
    void updateEntityFromShipmentConsoleEntityNotPresent() {
        CustomerBooking customerBooking = CustomerBooking.builder().build();
        customerBooking.setId(1L);

        when(customerBookingRepository.findById(any())).thenReturn(Optional.empty());

        assertThrows(DataRetrievalFailureException.class, () -> {
            customerBookingDao.save(customerBooking);
        });
    }

    @Test
    void updateEntityFromShipmentConsoleEntity() throws RunnerException {
        CustomerBooking customerBooking = CustomerBooking.builder().build();
        customerBooking.setId(1L);

        when(customerBookingRepository.findById(any())).thenReturn(Optional.of(customerBooking));
        when(customerBookingRepository.save(any())).thenReturn(customerBooking);

        assertEquals(customerBooking, customerBookingDao.updateEntityFromShipmentConsole(customerBooking));
    }

    @Test
    void updateEntityFromShipmentConsoleCatch() {
        CustomerBooking customerBooking = CustomerBooking.builder().build();
        customerBooking.setId(1L);

        when(customerBookingRepository.findById(any())).thenReturn(Optional.empty());

        assertThrows(RunnerException.class, () -> {
            customerBookingDao.updateEntityFromShipmentConsole(customerBooking);
        });
    }
}