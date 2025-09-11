package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.repository.interfaces.ICarrierBookingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
public class CarrierBookingDao implements ICarrierBookingDao {
    @Autowired
    private ICarrierBookingRepository carrierBookingRepository;

    @Override
    public CarrierBooking create(CarrierBooking carrierBooking) {
        return carrierBookingRepository.save(carrierBooking);
    }

    @Override
    public Optional<CarrierBooking> findById(Long id) {
        return carrierBookingRepository.findById(id);
    }

    @Override
    public Page<CarrierBooking> findAll(Specification<CarrierBooking> spec, Pageable pageable) {
        return carrierBookingRepository.findAll(spec, pageable);
    }


    @Override
    public CarrierBooking update(Long id, CarrierBooking request) {
        return carrierBookingRepository.save(request);
    }

    @Override
    public void delete(CarrierBooking carrierBooking) {
        carrierBookingRepository.delete(carrierBooking);
    }

    @Override
    public Long getTotalCarrierBookings() {
        return carrierBookingRepository.getTotalCarrierBookings(TenantContext.getCurrentTenant().longValue());
    }

    @Override
    public CarrierBooking save(CarrierBooking carrierBooking) {
        return carrierBookingRepository.save(carrierBooking);
    }

    @Override
    public CarrierBooking findByBookingNo(String bookingNo) {
        return carrierBookingRepository.findByBookingNo(bookingNo);
    }
}
