package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.SequenceIncrementor;

public interface ISequenceIncrementorDao {
    SequenceIncrementor save(SequenceIncrementor request);

    void delete(SequenceIncrementor request);
}
