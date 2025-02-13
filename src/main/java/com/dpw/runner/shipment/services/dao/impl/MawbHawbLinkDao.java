package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IMawbHawbLinkDao;
import com.dpw.runner.shipment.services.entity.MawbHawbLink;
import com.dpw.runner.shipment.services.repository.interfaces.IMawbHawbLinkRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Slf4j
public class MawbHawbLinkDao implements IMawbHawbLinkDao {
    @Autowired
    private IMawbHawbLinkRepository mawbHawbLinkRepository;

    @Override
    public MawbHawbLink save(MawbHawbLink mawbHawbLink) {
        return mawbHawbLinkRepository.save(mawbHawbLink);
    }

    @Override
    public Page<MawbHawbLink> findAll(Specification<MawbHawbLink> spec, Pageable pageable) {
        return mawbHawbLinkRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<MawbHawbLink> findById(Long id) {
        return mawbHawbLinkRepository.findById(id);
    }

    @Override
    public void delete(MawbHawbLink mawbHawbLink) {
        mawbHawbLinkRepository.delete(mawbHawbLink);
    }

    @Override
    public List<MawbHawbLink> findByMawbId(Long mawbId) {
        return mawbHawbLinkRepository.findByMawbId(mawbId);
    }

    @Override
    public List<MawbHawbLink> findByHawbId(Long hawbId) {
        return mawbHawbLinkRepository.findByHawbId(hawbId);
    }
}
