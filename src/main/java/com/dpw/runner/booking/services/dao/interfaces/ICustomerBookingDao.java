package com.dpw.runner.booking.services.dao.interfaces;

import com.dpw.runner.booking.services.entity.CustomerBooking;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;
import java.util.UUID;

public interface ICustomerBookingDao {

    CustomerBooking save(CustomerBooking customerBooking);

    Page<CustomerBooking> findAll(Specification<CustomerBooking> spec, Pageable pageable);

    Optional<CustomerBooking> findById(Long id);
    Optional<CustomerBooking> findByGuid(UUID id);

    void delete(CustomerBooking customerBooking);

    Optional<CustomerBooking> findByBookingNumber(String bookingNumber);

    int updateIsPlatformBookingCreated(Long id, Boolean isPlatformBookingCreated);
    int updateBillStatus(Long id, Boolean isBillCreated);

    Optional<CustomerBooking> findByOrderManagementId(String orderManagementId);
}
