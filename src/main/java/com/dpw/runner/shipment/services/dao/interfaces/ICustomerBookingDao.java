package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.CustomerBooking;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface ICustomerBookingDao {

    CustomerBooking save(CustomerBooking CustomerBooking);

    Page<CustomerBooking> findAll(Specification<CustomerBooking> spec, Pageable pageable);

    Optional<CustomerBooking> findById(Long id);

    void delete(CustomerBooking carrierDetails);

    CustomerBooking updateEntityFromShipmentConsole(CustomerBooking carrierDetails) throws Exception;

}
