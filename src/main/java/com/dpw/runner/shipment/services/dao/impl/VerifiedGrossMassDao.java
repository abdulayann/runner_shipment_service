package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IVerifiedGrossMassDao;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.repository.interfaces.IVerifiedGrossMassRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
public class VerifiedGrossMassDao implements IVerifiedGrossMassDao {

    private final IVerifiedGrossMassRepository verifiedGrossMassRepository;

    public VerifiedGrossMassDao(IVerifiedGrossMassRepository verifiedGrossMassRepository) {
        this.verifiedGrossMassRepository = verifiedGrossMassRepository;
    }

    @Override
    public Optional<VerifiedGrossMass> findById(Long id) {
        return verifiedGrossMassRepository.findById(id);
    }

    @Override
    public Page<VerifiedGrossMass> findAll(Specification<VerifiedGrossMass> spec, Pageable pageable) {
        return verifiedGrossMassRepository.findAll(spec, pageable);
    }

    @Override
    public VerifiedGrossMass update(Long id, VerifiedGrossMass verifiedGrossMass) {
        return verifiedGrossMassRepository.save(verifiedGrossMass);
    }

    @Override
    public void delete(Long id) {
        Optional<VerifiedGrossMass> verifiedGrossMass = verifiedGrossMassRepository.findById(id);
        if (verifiedGrossMass.isEmpty()) {
            throw new ValidationException("Invalid vgm id");
        }
        verifiedGrossMassRepository.deleteById(id);
    }

    @Override
    public VerifiedGrossMass save(VerifiedGrossMass verifiedGrossMass) {
        return verifiedGrossMassRepository.save(verifiedGrossMass);
    }

    @Override
    public boolean existsById(Long id) {
        return verifiedGrossMassRepository.existsById(id);
    }

    @Override
    public VerifiedGrossMass findByCarrierBookingNo(String carrierBookingNo) {
        return verifiedGrossMassRepository.findByCarrierBookingNo(carrierBookingNo);
    }
}
