package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ISequenceIncrementorDao;
import com.dpw.runner.shipment.services.commons.entity.SequenceIncrementor;
import com.dpw.runner.shipment.services.repository.interfaces.ISequenceIncrementorRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
@Slf4j
public class SequenceIncrementorDao implements ISequenceIncrementorDao {

    @Autowired
    ISequenceIncrementorRepository sequenceIncrementorRepository;

    @Override
    public SequenceIncrementor save(SequenceIncrementor request) {
        sequenceIncrementorRepository.save(request);
        return request;
    }

    @Override
    public void delete(SequenceIncrementor request) {
        sequenceIncrementorRepository.delete(request);
    }
}
