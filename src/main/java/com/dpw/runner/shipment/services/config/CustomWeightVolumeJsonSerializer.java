package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.jackson.JsonComponent;

@JsonComponent
@Generated
public class CustomWeightVolumeJsonSerializer {

    public static class CustomVolumeValueJsonSerializer extends CustomVolumeValueSerializer {
        @Autowired
        public CustomVolumeValueJsonSerializer(CommonUtils commonUtils) {
            super(commonUtils);
        }
    }

    public static class CustomWeightValueJsonSerializer extends CustomWeightValueSerializer {
        @Autowired
        public CustomWeightValueJsonSerializer(CommonUtils commonUtils) {
            super(commonUtils);
        }
    }
}
