package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.HblReleaseTypeMapping;

import java.util.List;

public interface IHblReleaseTypeMappingDao {
    HblReleaseTypeMapping save(HblReleaseTypeMapping hblReleaseTypeMapping);

    List<HblReleaseTypeMapping> findByReleaseTypeAndHblId(Long hblId, String releaseType);
}
