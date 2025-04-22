package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IHblReleaseTypeMappingDao;
import com.dpw.runner.shipment.services.entity.HblReleaseTypeMapping;
import com.dpw.runner.shipment.services.repository.interfaces.IHblReleaseTypeMappingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Repository
public class HblReleaseTypeMappingDao implements IHblReleaseTypeMappingDao {

    @Autowired
    private IHblReleaseTypeMappingRepository hblReleaseTypeMappingRepository;

    @Override
    public HblReleaseTypeMapping save(HblReleaseTypeMapping hblReleaseTypeMapping) {
        return hblReleaseTypeMappingRepository.save(hblReleaseTypeMapping);
    }

    @Override
    public List<HblReleaseTypeMapping> findByReleaseTypeAndHblId(Long hblId, String releaseType) {
        Optional<List<HblReleaseTypeMapping>> optional = hblReleaseTypeMappingRepository.findByHblIdAndReleaseType(hblId, releaseType);
        return optional.orElseGet(ArrayList::new);
    }
}
