package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IBookingCarriageDao;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingCarriageRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class BookingCarriageDao implements IBookingCarriageDao {
    @Autowired
    private IBookingCarriageRepository bookingCarriageRepository;

    @Override
    public BookingCarriage save(BookingCarriage bookingCarriage) {
        return bookingCarriageRepository.save(bookingCarriage);
    }

    @Override
    public Page<BookingCarriage> findAll(Specification<BookingCarriage> spec, Pageable pageable) {
        return bookingCarriageRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<BookingCarriage> findById(Long id) {
        return bookingCarriageRepository.findById(id);
    }

    @Override
    public void delete(BookingCarriage bookingCarriage) {
        bookingCarriageRepository.delete(bookingCarriage);
    }
}
